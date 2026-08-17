#' Data manifest: resolution and validation
#'
#' The manifest is the contract between the pipeline and the data. It records
#' every input and derived product, where it came from, whether it still exists
#' and what it should look like.
#'
#' The governing principle: a missing or wrong input must fail LOUDLY and say how
#' to obtain it. Most of this project's problems come from analyses that ran
#' quietly on the wrong thing. A pipeline that silently substitutes, skips or
#' half-runs is worse than one that stops.
#'
#' Source of truth: inst/manifest/data_manifest.csv

MANIFEST_CSV <- "inst/manifest/data_manifest.csv"

#' Availability values that mean "the bytes are on this machine"
AVAILABLE_LOCALLY <- c("mirrored", "in_repo", "reconstructed")


#' Read the data manifest
#'
#' @param path location of the manifest csv
#' @return data.frame, one row per catalogued product
read_manifest <- function(path = MANIFEST_CSV) {
  assert_config_exists(path, "Data manifest")
  m <- utils::read.csv(path, stringsAsFactors = FALSE)

  required <- c("id", "type", "group", "sensor", "kind", "filename_glob",
                "expected_count", "availability", "produced_by", "required_for",
                "notes", "resolved_path")
  missing <- setdiff(required, names(m))
  if (length(missing)) {
    stop("data_manifest.csv is missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(m$id)) {
    stop("data_manifest.csv has duplicate ids: ",
         paste(unique(m$id[duplicated(m$id)]), collapse = ", "), call. = FALSE)
  }
  m
}


#' One manifest entry by id
#'
#' @param id manifest id
#' @param manifest optional pre-read manifest
#' @return one-row data.frame
manifest_entry <- function(id, manifest = read_manifest()) {
  i <- which(manifest$id == id)
  if (length(i) != 1L) {
    stop("No manifest entry with id '", id, "'.\n",
         "  Every file the pipeline reads must be catalogued. Add a row to ",
         MANIFEST_CSV, " before referencing it.", call. = FALSE)
  }
  manifest[i, , drop = FALSE]
}


#' Expand a manifest resolved_path into concrete file paths
#'
#' Paths may contain a `{site}` placeholder, which expands over the configured
#' sites.
#'
#' @param id manifest id
#' @param sites site ids to expand over; defaults to all
#' @param manifest optional pre-read manifest
#' @return named character vector of paths, names are site ids where applicable
manifest_paths <- function(id, sites = NULL, manifest = read_manifest()) {
  e <- manifest_entry(id, manifest)
  p <- e$resolved_path

  if (is.na(p) || !nzchar(trimws(p))) {
    stop_unavailable(e)
  }
  if (!grepl("{site}", p, fixed = TRUE)) {
    return(stats::setNames(p, e$id))
  }
  if (is.null(sites)) sites <- read_sites()$site
  stats::setNames(
    vapply(sites, function(s) gsub("{site}", s, p, fixed = TRUE), character(1)),
    sites
  )
}


#' Refuse to proceed without a required input, and say how to get it
#'
#' This is the acquisition note. It is deliberately verbose: the reader is
#' someone whose pipeline just stopped, and the useful thing to tell them is
#' where the data went and who can supply it.
#'
#' @param e a one-row manifest data.frame
stop_unavailable <- function(e) {
  origin <- switch(
    e$availability,
    unknown_lost  = "Lost. Not present on the analysis server and not archived.",
    not_located   = "Catalogued but not found on this machine.",
    external      = "Held outside this repository.",
    zenodo        = "Published in the Zenodo archive.",
    paste0("Availability recorded as '", e$availability, "'.")
  )
  stop(
    "Required input '", e$id, "' is not available.\n",
    "  ", origin, "\n",
    "  Expected      : ", e$filename_glob, "  (x", e$expected_count, ")\n",
    "  Original home : ", e$original_location_hint, "\n",
    "  Produced by   : ", e$produced_by, "\n",
    "  Needed for    : ", e$required_for, "\n",
    "  Notes         : ", e$notes, "\n",
    "  This pipeline does not substitute or skip missing inputs.",
    call. = FALSE
  )
}


#' Assert that every file behind a manifest id exists
#'
#' @param id manifest id
#' @param sites site ids to check
#' @param manifest optional pre-read manifest
#' @return the validated paths, invisibly
assert_manifest_files <- function(id, sites = NULL, manifest = read_manifest()) {
  e <- manifest_entry(id, manifest)
  if (!e$availability %in% AVAILABLE_LOCALLY) stop_unavailable(e)

  paths <- manifest_paths(id, sites = sites, manifest = manifest)
  gone <- paths[!file.exists(paths)]
  if (length(gone)) {
    stop("Manifest entry '", id, "' says availability = '", e$availability,
         "' but ", length(gone), " of ", length(paths),
         " file(s) are absent:\n",
         paste0("    ", gone, collapse = "\n"), "\n",
         "  Either the mirror is incomplete (re-run tools/mirror-inputs.sh, then\n",
         "  tools/split-chm.sh) or the manifest is stale.",
         call. = FALSE)
  }
  invisible(paths)
}


#' Validate a raster against what the site configuration says it should be
#'
#' Checks CRS, band count and pixel size. Any mismatch is fatal: a silently
#' reprojected or resampled raster changes every extracted value downstream, and
#' that is exactly the class of error this refactor exists to catch.
#'
#' @param path raster path
#' @param site site id, used to look up expectations
#' @param expect_bands expected band count; NULL to skip
#' @param sites optional pre-read sites table
#' @param tolerance relative tolerance for the pixel-size comparison
#' @return a one-row data.frame describing what was found, invisibly
validate_raster <- function(path, site, expect_bands = NULL,
                            sites = read_sites(), tolerance = 1e-4) {
  if (!file.exists(path)) {
    stop("Raster not found: ", path, call. = FALSE)
  }
  s <- sites[sites$site == site, , drop = FALSE]
  if (nrow(s) != 1L) {
    stop("No site configuration for '", site, "'.", call. = FALSE)
  }

  r <- terra::rast(path)
  found_epsg  <- as.integer(terra::crs(r, describe = TRUE)$code)
  # nlyr() returns a double; identical(5, 5L) is FALSE, so coerce before compare.
  found_bands <- as.integer(terra::nlyr(r))
  found_res   <- terra::res(r)[1]

  problems <- character(0)
  if (is.na(found_epsg) || !identical(found_epsg, as.integer(s$epsg))) {
    problems <- c(problems, paste0(
      "CRS is ", if (is.na(found_epsg)) "UNDECLARED" else paste0("EPSG:", found_epsg),
      ", expected EPSG:", s$epsg))
  }
  if (!is.null(expect_bands) && !identical(found_bands, as.integer(expect_bands))) {
    problems <- c(problems, paste0(
      "band count is ", found_bands, ", expected ", expect_bands))
  }
  if (abs(found_res - s$pixel_m) / s$pixel_m > tolerance) {
    problems <- c(problems, paste0(
      "pixel size is ", signif(found_res, 6), " m, expected ",
      signif(s$pixel_m, 6), " m"))
  }

  if (length(problems)) {
    stop("Raster failed validation: ", path, "\n",
         paste0("    - ", problems, collapse = "\n"), "\n",
         "  sites.csv records what was measured off the mirror. A mismatch means\n",
         "  either the file changed or the config is wrong; both need explaining\n",
         "  before any model is fitted on it.",
         call. = FALSE)
  }

  invisible(data.frame(
    path = path, site = site, epsg = found_epsg,
    n_bands = found_bands, pixel_m = found_res,
    stringsAsFactors = FALSE
  ))
}


#' Validate a vector layer's CRS and geometry type
#'
#' Geometry type is checked because the field layers are POLYGON despite being
#' named `points` - a 30 cm buffer - and any port that assumes points silently
#' changes the extraction (finding 4.15).
#'
#' @param path vector path
#' @param site site id
#' @param expect_geometry expected geometry type, e.g. "POLYGON"
#' @param expect_features expected feature count; NULL to skip
#' @param sites optional pre-read sites table
#' @return a one-row data.frame describing what was found, invisibly
validate_vector <- function(path, site, expect_geometry = NULL,
                            expect_features = NULL, sites = read_sites()) {
  if (!file.exists(path)) {
    stop("Vector layer not found: ", path, call. = FALSE)
  }
  s <- sites[sites$site == site, , drop = FALSE]
  v <- sf::st_read(path, quiet = TRUE)

  found_epsg <- sf::st_crs(v)$epsg
  found_geom <- as.character(unique(sf::st_geometry_type(v)))
  found_n    <- nrow(v)

  problems <- character(0)
  if (is.na(found_epsg) || !identical(as.integer(found_epsg), as.integer(s$epsg))) {
    problems <- c(problems, paste0(
      "CRS is ", if (is.na(found_epsg)) "UNDECLARED (no .prj?)" else paste0("EPSG:", found_epsg),
      ", expected EPSG:", s$epsg))
  }
  if (!is.null(expect_geometry) && !all(found_geom %in% expect_geometry)) {
    problems <- c(problems, paste0(
      "geometry is ", paste(found_geom, collapse = "/"),
      ", expected ", paste(expect_geometry, collapse = "/")))
  }
  if (!is.null(expect_features) && !identical(found_n, as.integer(expect_features))) {
    problems <- c(problems, paste0(
      "feature count is ", found_n, ", expected ", expect_features))
  }

  if (length(problems)) {
    stop("Vector layer failed validation: ", path, "\n",
         paste0("    - ", problems, collapse = "\n"),
         call. = FALSE)
  }

  invisible(data.frame(
    path = path, site = site, epsg = as.integer(found_epsg),
    geometry = paste(found_geom, collapse = "/"), n_features = found_n,
    stringsAsFactors = FALSE
  ))
}


#' Summarise manifest availability
#'
#' Cheap target that makes the data position visible in the pipeline rather than
#' only in a document.
#'
#' @param manifest optional pre-read manifest
#' @return data.frame of counts by availability and type
manifest_summary <- function(manifest = read_manifest()) {
  as.data.frame(table(availability = manifest$availability,
                      type = manifest$type),
                stringsAsFactors = FALSE)
}
