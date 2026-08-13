# ---------------------------------------------------------------------------
# RECONSTRUCTED FILE - not part of the original published archive.
#
# `build_cube_5()`, `build_cube_5_CHM()` and `build_cube_5_CHM_NDVI()` are
# called by scripts/Shortcuts_to_data_specific_analysis/run_5*.R but exist in no
# repository (searched: this repo, TESS-Laboratory/slade-prosopis,
# GlennSlade/MLR3_pipeline, TESS-Laboratory/Glenn-Prosopis-ML). They are
# reconstructed here as band subsets of the canonical 11-band cube produced by
# R/build_cube.R.
#
# Basis for the reconstruction - manuscript section 2.5 defines four drone
# predictor stacks, and the benchmark/confusion output filenames use exactly
# these four tags:
#
#   tag             manuscript name          bands
#   5               Drone                    blue green red red_edge nir
#   5_CHM           Drone+CHM                + dsm
#   5_CHM_NDVI      Drone+CHM+NDVI           + NDVI
#   5_CHM_ALLVI     Drone+CHM+All VI         + MSAVI MSAVI2 MTVI NDVI SAVI
#                                            (= the full cube from build_cube())
#
# DELIBERATE DEVIATION FROM THE ORIGINAL. The original variants each wrote to
# data_out/<site>/<site>_stack.tif, so running two of them for the same site
# silently clobbered the first. Every commented-out "access the intermediate
# layer" line in the run_5*.R scripts points at that one shared path. Here each
# variant writes data_out/<site>/<site>_stack_<tag>.tif instead, so the four
# stacks coexist and targets can depend on them independently. Verify against
# the archived outputs before trusting any comparison across stacks.
#
# UNVERIFIED. No reference cube exists for the _5, _5_CHM or _5_CHM_NDVI
# variants, so these reconstructions cannot yet be checked against archived
# output. The full 11-band cube CAN be checked, against
# Glenn-Prosopis-ML/data_out/Bokspits_1/Bokspits_1_stack.tif.
# ---------------------------------------------------------------------------


#' Band composition of each named drone predictor stack
#'
#' Single source of truth for which layers belong to which stack. Referenced by
#' the reconstruction wrappers below and intended to be replaced in the rewrite
#' by inst/config/stacks.csv.
DRONE_STACK_BANDS <- list(
  "5"            = c("blue", "green", "red", "red_edge", "nir"),
  "5_CHM"        = c("blue", "green", "red", "red_edge", "nir", "dsm"),
  "5_CHM_NDVI"   = c("blue", "green", "red", "red_edge", "nir", "dsm", "NDVI"),
  "5_CHM_ALLVI"  = c("blue", "green", "red", "red_edge", "nir", "dsm",
                     "MSAVI", "MSAVI2", "MTVI", "NDVI", "SAVI")
)


#' Build a drone predictor stack by subsetting the canonical cube
#'
#' Reads data_out/<site>/<site>_stack.tif if it already exists, otherwise builds
#' it via build_cube(). Subsets to the requested bands and writes a tagged copy.
#'
#' @param site_name character(1), the site prefix.
#' @param tag character(1), one of names(DRONE_STACK_BANDS).
#' @param data_dir default "data_in". Parent directory of input data.
#' @param out_data_dir default "data_out". Parent directory of output data.
#' @return A SpatRaster of the subset stack.
build_cube_subset <- function(site_name,
                              tag,
                              data_dir = "data_in",
                              out_data_dir = "data_out") {

  if (!tag %in% names(DRONE_STACK_BANDS)) {
    stop("Unknown stack tag '", tag, "'. Expected one of: ",
         paste(names(DRONE_STACK_BANDS), collapse = ", "), call. = FALSE)
  }
  want <- DRONE_STACK_BANDS[[tag]]

  out_dir <- file.path(out_data_dir, site_name)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  full_path <- file.path(out_dir, paste0(site_name, "_stack.tif"))

  if (file.exists(full_path)) {
    cube <- terra::rast(full_path)
  } else {
    message("Full cube not found at ", full_path, " - building it first.")
    cube <- build_cube(site_name = site_name,
                       data_dir = data_dir,
                       out_data_dir = out_data_dir)
  }

  missing <- setdiff(want, names(cube))
  if (length(missing)) {
    stop("Cube for '", site_name, "' is missing required layer(s): ",
         paste(missing, collapse = ", "),
         "\n  Cube has: ", paste(names(cube), collapse = ", "),
         call. = FALSE)
  }

  out_path <- file.path(out_dir, paste0(site_name, "_stack_", tag, ".tif"))

  terra::writeRaster(cube[[want]],
                     out_path,
                     wopt = list(gdal = c("COMPRESS=DEFLATE")),
                     overwrite = TRUE)
}


#' Drone stack: 5 multispectral bands only
build_cube_5 <- function(site_name, data_dir = "data_in",
                         out_data_dir = "data_out") {
  build_cube_subset(site_name, "5", data_dir, out_data_dir)
}

#' Drone stack: 5 multispectral bands + canopy height model
build_cube_5_CHM <- function(site_name, data_dir = "data_in",
                             out_data_dir = "data_out") {
  build_cube_subset(site_name, "5_CHM", data_dir, out_data_dir)
}

#' Drone stack: 5 multispectral bands + CHM + NDVI
build_cube_5_CHM_NDVI <- function(site_name, data_dir = "data_in",
                                  out_data_dir = "data_out") {
  build_cube_subset(site_name, "5_CHM_NDVI", data_dir, out_data_dir)
}

#' Drone stack: 5 multispectral bands + CHM + all vegetation indices
#'
#' Identical to the canonical build_cube(), exposed under the tag name for
#' symmetry with the other three.
build_cube_5_CHM_ALLVI <- function(site_name, data_dir = "data_in",
                                   out_data_dir = "data_out") {
  build_cube_subset(site_name, "5_CHM_ALLVI", data_dir, out_data_dir)
}


# ---------------------------------------------------------------------------
# Satellite cubes
#
# `build_cube_WV2()`, `build_cube_Planet()`, `build_cube_S2()` and
# `build_cube_LS8_T()` are referenced by the runWV2/runPlanet/runS2/runLSTR
# shortcuts. Only a near-miss survives: R/Temp_build_cube_wv2.R defines
# `build_cube2()`, a 4-band (blue/green/red/nir) version of build_cube() reading
# "<site>_Corrected.tif". It is called nowhere, and is almost certainly the
# WorldView-2 cube builder.
#
# build_cube_sat() below generalises it. It is a RECONSTRUCTION, not a recovery:
# the band count and the input filename suffix are inferred from the
# preprocessing scripts, not read off surviving code.
# ---------------------------------------------------------------------------

#' Build a satellite predictor cube
#'
#' @param site_name character(1), the site prefix.
#' @param bands character vector of band names, in file order.
#' @param stack_suffix character(1), suffix of the source raster, e.g.
#'   "_Corrected.tif".
#' @param vi_names vegetation index rasters to stack alongside, expected at
#'   <data_dir>/<site>/<site>_<vi>.tif.
#' @param data_dir default "data_in".
#' @param out_data_dir default "data_out".
#' @return A SpatRaster.
build_cube_sat <- function(site_name,
                           bands = c("blue", "green", "red", "nir"),
                           stack_suffix = "_Corrected.tif",
                           vi_names = c("MSAVI", "MSAVI2", "MTVI", "NDVI", "SAVI"),
                           data_dir = "data_in",
                           out_data_dir = "data_out") {

  out_dir <- file.path(out_data_dir, site_name)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  base_dir <- file.path(data_dir, site_name)

  aoi <- terra::vect(file.path(base_dir, paste0(site_name, "_clip.shp")))

  ms <- terra::rast(file.path(base_dir, paste0(site_name, stack_suffix)))
  if (terra::nlyr(ms) != length(bands)) {
    stop("Expected ", length(bands), " bands in ", site_name, stack_suffix,
         " but found ", terra::nlyr(ms), ".", call. = FALSE)
  }
  names(ms) <- bands

  read_n_rename <- function(p, .name) {
    r <- terra::rast(p)
    names(r) <- .name
    r
  }

  vi_paths <- file.path(base_dir, paste0(site_name, "_", vi_names, ".tif"))

  vi_stack <- purrr::map2(vi_paths, vi_names, read_n_rename) |>
    terra::rast() |>
    terra::project(ms)

  all_bands <- c(ms, vi_stack) |> terra::mask(aoi)

  terra::writeRaster(all_bands,
                     file.path(out_dir, paste0(site_name, "_stack.tif")),
                     wopt = list(gdal = c("COMPRESS=DEFLATE")),
                     overwrite = TRUE)
}


#' WorldView-2 cube: 4 bands + 5 vegetation indices
build_cube_WV2 <- function(site_name, data_dir = "data_in",
                           out_data_dir = "data_out") {
  build_cube_sat(site_name,
                 bands = c("blue", "green", "red", "nir"),
                 stack_suffix = "_Corrected.tif",
                 data_dir = data_dir, out_data_dir = out_data_dir)
}

#' PlanetScope cube: 4 bands + 5 vegetation indices
build_cube_Planet <- function(site_name, data_dir = "data_in",
                              out_data_dir = "data_out") {
  build_cube_sat(site_name,
                 bands = c("blue", "green", "red", "nir"),
                 stack_suffix = "_Corrected.tif",
                 data_dir = data_dir, out_data_dir = out_data_dir)
}

#' Sentinel-2 cube: 4 bands + 5 vegetation indices + red-edge index
#'
#' Sentinel-2 additionally carries REVI, computed in
#' Analysis/Extraction/S2/S2_Boravast_VI.R from band B07 resampled 20 m -> 10 m.
build_cube_S2 <- function(site_name, data_dir = "data_in",
                          out_data_dir = "data_out") {
  build_cube_sat(site_name,
                 bands = c("blue", "green", "red", "nir"),
                 stack_suffix = "_stack.tif",
                 vi_names = c("MSAVI", "MSAVI2", "MTVI", "NDVI", "SAVI", "REVI"),
                 data_dir = data_dir, out_data_dir = out_data_dir)
}
