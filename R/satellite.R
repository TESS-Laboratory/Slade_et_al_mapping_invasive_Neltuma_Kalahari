#' Satellite arm - WV2 first (manuscript sections 2.4 and 3.2)
#'
#' The WV2 inputs all survive in the mirror (finding 7.32): the co-registered
#' mosaic `WV2_Corrected.tif` (the manifest's former "NO PRODUCER FOUND"), the
#' five VI rasters derived from it, and - crucially - Glen's actual training
#' extraction `WV2_equal_class_size_500_train_95.shp`: WV2 pixel polygons whose
#' majority drone class exceeded 95% purity, 500 per class except S.mellifera,
#' which yields only 400 at that threshold.
#'
#' Two design points, both evidenced rather than assumed:
#'
#'   - The reported run's confusion column sums are exactly 400 x 10 repeats for
#'     ALL six classes (finding 7.19), so the effective training size was 400
#'     per class: the 500-request was balanced down to the rarest class. We
#'     reproduce that with `balance_classes()`, seeded.
#'   - The extraction shapefile carries NO band values (unlike Planet/S2), so
#'     features are extracted here from the cube over the pixel polygons -
#'     exactly what the original's build_ml_df_WV2e did against WV2e_stack.tif.
#'
#' All six rasters sit on the identical grid (10330 x 19516, 1.6 m,
#' EPSG:32734) - verified 2026-09-15, and re-verified on every build below, so
#' the drone arm's footprint trap (R/cubes.R preamble) cannot silently recur.

WV2_DIR <- "data-in/wv2/glenn"

# Band order follows the original Temp_build_cube_wv2.R: the four corrected
# reflectance bands, then the VIs in its add_band_names order. Lowercase to
# match the drone stacks. Finding 4.10's caveat carries over: the file NAMED
# MSAVI implements MSAVI2 and MTVI implements MTVI2; names are labels of the
# files as shipped, not endorsements of the formulas.
WV2_BANDS <- c("blue", "green", "red", "nir",
               "msavi", "msavi2", "mtvi", "ndvi", "savi")

#' The six WV2 raster files, in cube band order
#'
#' Returned as a vector for `format = "file"` tracking, so a re-mirrored raster
#' invalidates the cube.
#'
#' @param dir directory holding the mirrored WV2 products
#' @return character vector of file paths
wv2_raster_files <- function(dir = WV2_DIR) {
  files <- file.path(dir, c(
    "WV2_Corrected.tif",
    "WV2_MSAVI.tif", "WV2_MSAVI2.tif", "WV2_MTVI.tif",
    "WV2_NDVI.tif", "WV2_SAVI.tif"
  ))
  missing <- files[!file.exists(files)]
  if (length(missing)) {
    stop("WV2 raster(s) not mirrored:\n",
         paste0("    ", missing, collapse = "\n"), "\n",
         "  Run: sudo tools/mirror-results.sh --with-satellite",
         call. = FALSE)
  }
  files
}


#' Assemble a satellite predictor cube as a VRT
#'
#' Same mechanics as the drone `build_cube()`: `-separate` expands the
#' multi-band corrected mosaic into consecutive bands, the extent is pinned to
#' the first source, and the pin is verified rather than trusted. Unlike the
#' drone VIs the WV2 sources share one grid, so the pin should be a no-op -
#' which is exactly why it is asserted.
#'
#' @param srcs source rasters in band order, first one defines the grid
#' @param bands band names for the assembled cube
#' @param sensor sensor id, used for the output name and error messages
#' @param out_dir where to write the VRT
#' @return path to the written VRT
build_satellite_cube <- function(srcs, bands, sensor,
                                 out_dir = "data-out/cubes") {
  ref <- terra::rast(srcs[1])
  e   <- as.vector(terra::ext(ref))

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  vrt <- file.path(out_dir, paste0(sensor, "__all.vrt"))

  sf::gdal_utils(
    util = "buildvrt",
    source = srcs,
    destination = vrt,
    options = c(
      "-separate",
      "-te", sprintf("%.10f", c(e["xmin"], e["ymin"], e["xmax"], e["ymax"])),
      "-resolution", "user",
      "-tr", sprintf("%.10f", terra::res(ref)),
      "-r", "nearest"
    ),
    quiet = TRUE
  )

  set_vrt_band_names(vrt, bands)

  cube <- terra::rast(vrt)
  problems <- character(0)
  if (!identical(dim(cube)[1:2], dim(ref)[1:2])) {
    problems <- c(problems, paste0(
      "dimensions are ", paste(dim(cube)[1:2], collapse = "x"),
      ", reference grid is ", paste(dim(ref)[1:2], collapse = "x")))
  }
  if (!isTRUE(all.equal(as.vector(terra::ext(cube)), as.vector(terra::ext(ref)),
                        tolerance = 1e-6))) {
    problems <- c(problems, "extent differs from the reference grid")
  }
  if (!identical(as.integer(terra::nlyr(cube)), length(bands))) {
    problems <- c(problems, paste0(
      "band count is ", terra::nlyr(cube), ", expected ", length(bands)))
  }
  if (length(problems)) {
    stop("Satellite cube for ", sensor, " failed verification:\n",
         paste0("    - ", problems, collapse = "\n"), call. = FALSE)
  }
  vrt
}


#' Declare the CRS on the WV2 study-area boundary
#'
#' `WV2_clip.shp` ships without a `.prj` (finding 7.13), but its coordinates
#' are unambiguously UTM 34S - the bbox sits inside the corrected mosaic's
#' extent. The CRS is declared, never transformed, and the result is written
#' as FlatGeobuf: new vector products do not get minted as shapefiles
#' (decision 2026-09-15 [HUGH]; the wholesale .shp conversion waits for the
#' next refactor phase).
#'
#' @param shp path to the boundary shapefile
#' @param epsg the CRS to declare
#' @param out output path
#' @return `out`
fix_wv2_aoi <- function(shp, epsg, out = "data-out/wv2/wv2_aoi.fgb") {
  v <- sf::st_read(shp, quiet = TRUE)
  if (is.na(sf::st_crs(v))) {
    v <- sf::st_set_crs(v, epsg)
  } else if (!identical(sf::st_crs(v), sf::st_crs(epsg))) {
    stop("WV2 AOI has grown a CRS that is not EPSG:", epsg,
         " - re-check finding 7.13 before trusting this layer.", call. = FALSE)
  }
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(v, out, delete_dsn = TRUE, quiet = TRUE)
  out
}


#' Class areas of one surface inside one boundary
#'
#' @param class_tif path to a class raster (any resolution)
#' @param aoi a SpatVector boundary in the same CRS
#' @return data.frame of Type and area_ha for classes present
masked_class_areas <- function(class_tif, aoi) {
  r <- terra::rast(class_tif)[[1]]
  r <- terra::mask(terra::crop(r, aoi), aoi)
  f <- terra::freq(r)
  data.frame(Type = as.integer(f$value),
             area_ha = f$count * prod(terra::res(r)) / 1e4)
}


#' Drone vs WV2 class areas for one site - the Table S10 producer
#'
#' The original's Table S10 compared WV2 areas against SMOOTHED drone maps,
#' a reference from which part of the sparse Neltuma had already been erased
#' (finding 7.31). All four surface combinations are produced here so the
#' reference-side choice is an explicit authorial decision [ANDY], not an
#' artefact of what happened to be on disk.
#'
#' @param site drone site id
#' @param aoi_path the drone site boundary
#' @param drone named list: raw and smoothed drone prediction paths
#' @param wv2 named list: raw and smoothed WV2 prediction paths
#' @return long data.frame: site, sensor, surface, Type, area_ha
compare_site_surfaces <- function(site, aoi_path, drone, wv2) {
  aoi <- terra::vect(aoi_path)
  sets <- list(
    drone_raw      = drone$raw[1],      drone_smoothed = drone$smoothed[1],
    wv2_raw        = wv2$raw[1],        wv2_smoothed   = wv2$smoothed[1]
  )
  out <- lapply(names(sets), function(nm) {
    a <- masked_class_areas(sets[[nm]], aoi)
    parts <- strsplit(nm, "_")[[1]]
    cbind(site = site, sensor = parts[1], surface = parts[2], a)
  })
  do.call(rbind, out)
}


#' Purity extraction: drone class fractions per WV2 pixel, one site
#'
#' The re-derivation of extract_WV2_pixel.R against OUR drone surfaces: for
#' each WV2 pixel polygon over a drone site, the majority drone class and the
#' fractional cover of every class. Run twice per site - against the raw and
#' the smoothed surface - because finding 7.31 showed the smoothing deletes
#' exactly the sparse Neltuma pixels a purity filter would otherwise admit,
#' and the original extracted from surfaces whose smoothing status is part of
#' the open Table S10 question.
#'
#' @param class_tif drone class raster path
#' @param grid_path WV2 pixel-grid shapefile for this site
#' @param site site id, recorded in the output
#' @return sf: site, Type (majority class), frac_* columns, polygon geometry
purity_extract <- function(class_tif, grid_path, site) {
  r    <- terra::rast(class_tif[1])[[1]]
  grid <- sf::st_read(grid_path, quiet = TRUE)

  maj <- exactextractr::exact_extract(r, grid, "majority", progress = FALSE)
  fr  <- exactextractr::exact_extract(r, grid, "frac", progress = FALSE)
  fr[is.na(fr)] <- 0

  out <- sf::st_sf(site = site, Type = as.integer(maj), fr,
                   geometry = sf::st_geometry(grid))
  out[!is.na(out$Type), , drop = FALSE]
}


#' Filter a combined purity extraction into a training layer
#'
#' Keeps pixels whose OWN-class fraction exceeds the purity threshold - the
#' same criterion as the original's per-class `filter(frac_c > A)` chain,
#' expressed once. Classes outside the sensor's roster (7.32: WV2 trains on
#' {1,2,3,5,6,7}; Gnidia and the rare classes never reach satellite scale) are
#' dropped. Written as FlatGeobuf, and balancing to class size happens later
#' on the extracted table, mirroring the archived arm.
#'
#' @param ext combined sf from `purity_extract()` across sites
#' @param purity own-class fraction threshold, from sensors.csv
#' @param keep_classes the sensor's class roster, from satellite.yml
#' @param out output path
#' @return `out`
build_purity_layer <- function(ext, purity, keep_classes, out) {
  frac_cols <- grep("^frac_", names(ext), value = TRUE)
  fr <- as.matrix(sf::st_drop_geometry(ext)[, frac_cols, drop = FALSE])

  own_col <- match(paste0("frac_", ext$Type), colnames(fr))
  own <- fr[cbind(seq_len(nrow(ext)), own_col)]

  keep <- !is.na(own) & own > purity & ext$Type %in% keep_classes
  v <- ext[keep, c("site", "Type"), drop = FALSE]
  if (!nrow(v)) {
    stop("Purity filter at ", purity, " kept zero pixels - wrong surface, ",
         "wrong grid, or a threshold typo.", call. = FALSE)
  }

  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(v, out, delete_dsn = TRUE, quiet = TRUE)
  out
}


#' Balance a training table to equal class sizes
#'
#' The archived WV2 extraction holds 500 per class but S.mellifera caps at 400
#' at 95% purity, and the reported run's confusion sums show it trained on 400
#' per class - balanced to the rarest class (findings 7.19, 7.32). Seeded here,
#' unlike the original's bare `sample()`, so the subsample is reproducible
#' within this pipeline even though the original's exact rows are not
#' recoverable.
#'
#' @param df a training table from `build_training_table()`
#' @param n rows per class; default the size of the rarest class
#' @param cap upper bound on n (the sensor's class size from sensors.csv), so
#'   a re-derived extraction with plentiful pure pixels still trains at the
#'   original's scale rather than swamping it
#' @param seed RNG seed, from resampling.yml
#' @return the balanced table, row order re-sorted by Type
balance_classes <- function(df, n = NULL, cap = NULL, seed) {
  counts <- table(df$Type)
  if (is.null(n)) n <- min(counts)
  if (!is.null(cap)) n <- min(n, cap)
  short <- names(counts)[counts < n]
  if (length(short)) {
    stop("Cannot balance to ", n, " per class; short class(es): ",
         paste0(short, " (", counts[short], ")", collapse = ", "),
         call. = FALSE)
  }
  set.seed(seed)
  keep <- unlist(lapply(split(seq_len(nrow(df)), df$Type),
                        function(idx) sample(idx, n)),
                 use.names = FALSE)
  out <- df[sort(keep), , drop = FALSE]
  rownames(out) <- NULL
  out
}
