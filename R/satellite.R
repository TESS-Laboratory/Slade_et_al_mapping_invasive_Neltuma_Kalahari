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



#' Write an sf layer as FlatGeobuf without the first-write GDAL warning
#'
#' `delete_dsn = TRUE` on a path that does not exist yet makes GDAL warn; only
#' ask for the delete when there is something to delete.
#'
#' @param v sf object
#' @param out output path
#' @return `out`
write_fgb <- function(v, out) {
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(v, out, delete_dsn = file.exists(out), quiet = TRUE)
  out
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
  write_fgb(v, out)
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

  write_fgb(v, out)
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


#' Hexagonal (or square) analysis grid over the study area
#'
#' The original's hex layers lived in a hand-made GIS_aggregate folder that is
#' lost (manifest: hex_grids, unknown_lost), so the tessellation is
#' regenerated with sf. Cover statistics over ~5 ha cells are insensitive to
#' the tessellation origin; exact cell boundaries are not reproduced and no
#' claim depends on them.
#'
#' @param aoi_path study-area boundary (the CRS-fixed .fgb)
#' @param cell_m cell size in metres (hexagon short diameter)
#' @param out output path (.fgb)
#' @param square FALSE for hexagons (Fig 8B), TRUE for the square prevalence
#'   grid (Fig 8A reads "100 m grid cells")
#' @return `out`
make_analysis_grid <- function(aoi_path, cell_m, out, square = FALSE) {
  aoi <- sf::st_read(aoi_path, quiet = TRUE)
  g <- sf::st_make_grid(aoi, cellsize = cell_m, square = square)
  g <- g[lengths(sf::st_intersects(g, aoi)) > 0]
  v <- sf::st_sf(cell_id = seq_along(g), geometry = g)
  write_fgb(v, out)
}


#' Neltuma cover and invasion phase per grid cell
#'
#' Cover is computed from BOTH the raw and the smoothed surface: finding 7.31
#' showed the modal filter erases sparse Neltuma, and the phase thresholds
#' bottom out at 0.1% cover - exactly the range the filter deletes - so the
#' choice of input surface plausibly moves cells across the Pre-Incursion /
#' Initial Incursion boundary and must be measured.
#'
#' Threshold conventions from Table S8: Dominance > 15, Expansion (1.5, 15],
#' Initial Incursion [0.1, 1.5), Pre-Incursion < 0.1. The published table
#' leaves the boundary membership ambiguous ("1.5 - 15%" vs "0.1 - 1.5%");
#' boundaries here go to the LOWER phase, which only matters for cells landing
#' exactly on a threshold.
#'
#' @param raw_tif,smooth_tif class raster paths
#' @param grid_path analysis grid (.fgb)
#' @param neltuma_code integer class code for Neltuma
#' @param th thresholds list: dominance, expansion, incursion (percent)
#' @param out output layer path (.fgb)
#' @return `out`
build_phase_layer <- function(raw_tif, smooth_tif, grid_path, neltuma_code,
                              th, out) {
  grid <- sf::st_read(grid_path, quiet = TRUE)

  cover_of <- function(tif) {
    r <- terra::rast(tif[1])[[1]] == neltuma_code
    100 * exactextractr::exact_extract(r, grid, "mean", progress = FALSE)
  }
  phase_of <- function(cover) {
    cut(cover,
        breaks = c(-Inf, th$incursion, th$expansion, th$dominance, Inf),
        labels = c("Pre-Incursion", "Initial Incursion", "Expansion",
                   "Dominance"),
        right = FALSE)
  }

  grid$cover_raw    <- cover_of(raw_tif)
  grid$cover_smooth <- cover_of(smooth_tif)
  grid$phase_raw    <- phase_of(grid$cover_raw)
  grid$phase_smooth <- phase_of(grid$cover_smooth)

  write_fgb(grid, out)
}


#' Phase area accounting - the Table 1 producer
#'
#' @param layer_path output of `build_phase_layer()`
#' @return data.frame: surface, phase, n_cells, area_ha, pct_of_area
phase_summary <- function(layer_path) {
  g <- sf::st_read(layer_path, quiet = TRUE)
  area_ha <- as.numeric(sf::st_area(g)) / 1e4
  out <- lapply(c(raw = "phase_raw", smooth = "phase_smooth"), function(col) {
    a <- tapply(area_ha, g[[col]], sum, default = 0)
    data.frame(phase = names(a), n_cells = as.integer(table(g[[col]])),
               area_ha = as.numeric(a),
               pct_of_area = 100 * as.numeric(a) / sum(area_ha))
  })
  cbind(surface = rep(names(out), each = nrow(out[[1]])),
        do.call(rbind, out), row.names = NULL)
}


#' Plant-scale validation - the Table S9 producer
#'
#' `All_points_buffered_additional.shp` (mirrored with the WV2 set, no .prj,
#' same CRS situation as WV2_clip) is the seven sites' field train+val point
#' buffers plus 134 "additional" digitised points of only bare/grass/woody
#' classes - matching Table S9's "every Neltuma plant ... and a representative
#' selection of grass and bare ground". Neltuma count is 214 against the
#' manuscript's n = 184; the difference is RECORDED, not resolved (finding
#' 7.33) - plants outside the classified footprints drop out below, which may
#' or may not close the gap.
#'
#' Table S9 extracted "the majority classification from a 20 cm radius circle
#' at each plant's centre"; these buffers average ~1 m2. The layer is used as
#' shipped rather than re-buffered - the majority over either disc differs
#' only where a plant straddles a class boundary.
#'
#' @param site drone site id
#' @param points_path the buffered points shapefile
#' @param aoi_path this site's boundary
#' @param raw_tif,smooth_tif this site's class surfaces
#' @param epsg CRS to declare on the layer
#' @return data.frame: site, Type, pred_raw, pred_smooth for points on-site
plant_scale_site <- function(site, points_path, aoi_path, raw_tif, smooth_tif,
                             epsg = 32734L) {
  v <- sf::st_read(points_path, quiet = TRUE)
  if (is.na(sf::st_crs(v))) v <- sf::st_set_crs(v, epsg)

  # Membership by the site AOI, not the raster bounding box: adjacent sites'
  # rasters overlap at their margins and a bbox test counted boundary plants
  # twice (234 Neltuma rows from a 214-plant layer on the first run).
  aoi <- sf::st_read(aoi_path, quiet = TRUE)
  if (is.na(sf::st_crs(aoi))) aoi <- sf::st_set_crs(aoi, epsg)
  inside <- lengths(sf::st_intersects(v, sf::st_union(aoi))) > 0
  v <- v[inside, , drop = FALSE]
  r_raw <- terra::rast(raw_tif[1])[[1]]
  if (!nrow(v)) {
    return(data.frame(site = character(0), Type = integer(0),
                      pred_raw = integer(0), pred_smooth = integer(0)))
  }

  maj <- function(r) as.integer(
    exactextractr::exact_extract(r, v, "majority", progress = FALSE))
  data.frame(site = site, Type = as.integer(v$Type),
             pred_raw = maj(r_raw),
             pred_smooth = maj(terra::rast(smooth_tif[1])[[1]]))
}


#' Per-class plant-scale accuracy, both surfaces
#'
#' @param df combined output of `plant_scale_site()`
#' @return data.frame: surface, Type, n, n_correct, accuracy
plant_scale_summary <- function(df) {
  # Uniqueness comes from plant_scale_site's AOI-membership test - the site
  # AOIs do not overlap, so no further deduplication is needed here.
  out <- lapply(c(raw = "pred_raw", smooth = "pred_smooth"), function(col) {
    ok <- !is.na(df[[col]])
    agg <- aggregate(list(n = ok, n_correct = ok & df[[col]] == df$Type),
                     by = list(Type = df$Type), FUN = sum)
    agg$accuracy <- agg$n_correct / agg$n
    agg
  })
  cbind(surface = rep(names(out), vapply(out, nrow, 1L)),
        do.call(rbind, out), row.names = NULL)
}


#' Pixel-level WV2 vs drone confusion - the Table S10 matrix itself
#'
#' The original's S10 is a confusion matrix over WV2 pixels inside the drone
#' sites: rows = WV2 class, columns = majority drone class. The purity
#' extraction already holds the drone majority per WV2 pixel polygon, so the
#' matrix is one more extraction of the WV2 class over the same polygons.
#'
#' @param ext combined sf from `purity_extract()` (drone majority in `Type`)
#' @param wv2_tif WV2 class raster path
#' @param keep_classes the sensor's class roster
#' @return data.frame: wv2_class, drone_class, n_pixels
wv2_drone_confusion <- function(ext, wv2_tif, keep_classes) {
  r <- terra::rast(wv2_tif[1])[[1]]
  wv2 <- as.integer(exactextractr::exact_extract(r, ext, "majority",
                                                  progress = FALSE))
  ok <- !is.na(wv2) & ext$Type %in% keep_classes
  tab <- table(wv2_class = wv2[ok], drone_class = ext$Type[ok])
  out <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(out) <- c("wv2_class", "drone_class", "n_pixels")
  out$wv2_class <- as.integer(out$wv2_class)
  out$drone_class <- as.integer(out$drone_class)
  out[out$n_pixels > 0, , drop = FALSE]
}
