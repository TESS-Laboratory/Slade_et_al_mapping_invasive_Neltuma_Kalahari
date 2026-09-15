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
#' @param seed RNG seed, from resampling.yml
#' @return the balanced table, row order re-sorted by Type
balance_classes <- function(df, n = NULL, seed) {
  counts <- table(df$Type)
  if (is.null(n)) n <- min(counts)
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
