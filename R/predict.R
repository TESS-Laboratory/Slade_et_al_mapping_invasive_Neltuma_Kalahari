#' Landscape prediction
#'
#' One classified surface per site: the winning learner for the configured
#' stack (best_models), retrained on all of that site's training data with its
#' tuned configuration, then predicted over the full cube.
#'
#' Two constraints inherited from the data, both enforced here:
#'   - The VI bands carry data OUTSIDE the AOI mask, where the reflectance and
#'     CHM bands are nodata (finding 7.20). Any pixel with an incomplete
#'     predictor row is returned as NA rather than handed to the model.
#'   - The output is additionally masked to the site AOI, so the surface's
#'     footprint is the surveyed area, not the VI rasters' larger extent.
#'
#' Outputs per site: <site>__<tag>_class.tif (integer Type codes) and
#' <site>__<tag>_prob.tif (one layer per class, named by code). Probabilities
#' are the foundation for the conformal treatment deferred to the next refactor.

#' Predict one site's landscape surface
#'
#' @param cube_path VRT of the configured stack
#' @param aoi_path site AOI shapefile (first element of the tracked file set)
#' @param training the site's training table for this stack
#' @param best one row of best_models for this site and stack
#' @param resampling the resolved config (for the winner's spec and settings)
#' @param tuned_configs named list: learner id -> tuned config for this task
#' @param site,tag ids
#' @param aggregate integer >= 1; >1 predicts on an aggregated cube (fast profile)
#' @param out_dir output directory
#' @return character vector of written paths
predict_site <- function(cube_path, aoi_path, training, best, resampling,
                         tuned_configs, site, tag, aggregate = 1L,
                         out_dir = "data-out/predict") {
  data.table::setDTthreads(1L)
  stopifnot(nrow(best) == 1L)
  winner <- best$learner

  spec   <- learner_spec(resampling, winner)
  shared <- eval_settings(resampling)
  config <- tuned_configs[[winner]]

  learner <- bare_learner(spec, shared)
  if (!is.null(config)) {
    keep <- config[names(config) %in% learner$param_set$ids()]
    learner$param_set$set_values(.values = keep)
  }

  # A plain TaskClassif, not TaskClassifST: the spatial wrapper exists for
  # resampling, which does not happen here, and it demands coordinate columns
  # in every predict_newdata() call - which raster pixels do not have.
  feats <- setdiff(names(training), c("Type", "site", "tag", "x", "y"))
  task <- mlr3::as_task_classif(training[, c("Type", feats)], target = "Type",
                                id = paste0(site, "__", tag, "__final"))
  set.seed(shared$seed)
  learner$train(task)

  cube <- terra::rast(cube_path)
  if (aggregate > 1L) {
    cube <- terra::aggregate(cube, fact = aggregate, fun = "mean", na.rm = FALSE)
  }
  if (!all(feats %in% names(cube))) {
    stop("Cube ", basename(cube_path), " lacks feature band(s): ",
         paste(setdiff(feats, names(cube)), collapse = ", "), call. = FALSE)
  }
  cube <- cube[[feats]]

  lvls <- task$class_names
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Row-wise prediction wrapper for terra::predict. Incomplete rows (VI data
  # outside the reflectance footprint, 7.20) come back as NA, never predicted.
  wrap <- function(model, dat, ...) {
    out <- matrix(NA_real_, nrow = nrow(dat), ncol = 1L + length(lvls))
    ok <- stats::complete.cases(dat)
    if (any(ok)) {
      pr <- model$predict_newdata(dat[ok, , drop = FALSE])
      out[ok, 1L] <- as.integer(as.character(pr$response))
      out[ok, -1L] <- pr$prob[, lvls, drop = FALSE]
    }
    out
  }

  # Prediction is the "few heavy targets" case: one 75-224M pixel surface per
  # site on an otherwise idle machine, so per-target parallelism pays here where
  # it did not for the fits (7.24). terra tiles the raster across forked
  # workers; svm predict cost is O(support vectors x pixels) and struizendam_2
  # took ~2h single-threaded.
  n_cores <- as.integer(Sys.getenv("NELTUMA_PREDICT_CORES", "8"))
  pred <- terra::predict(cube, learner, fun = wrap, na.rm = FALSE,
                         cores = if (aggregate > 1L) 1L else n_cores)
  names(pred) <- c("class", paste0("prob_", lvls))

  aoi <- terra::vect(aoi_path)
  pred <- terra::mask(pred, aoi)

  suffix <- if (aggregate > 1L) paste0("_agg", aggregate) else ""
  class_path <- file.path(out_dir, paste0(site, "__", tag, suffix, "_class.tif"))
  prob_path  <- file.path(out_dir, paste0(site, "__", tag, suffix, "_prob.tif"))

  terra::writeRaster(pred[["class"]], class_path, overwrite = TRUE,
                     datatype = "INT1U", gdal = c("COMPRESS=LZW", "TILED=YES"),
                     NAflag = 255)
  terra::writeRaster(pred[[-1L]], prob_path, overwrite = TRUE,
                     gdal = c("COMPRESS=LZW", "PREDICTOR=3", "TILED=YES"))
  c(class_path, prob_path)
}


#' Summarise a predicted surface
#'
#' Class areas in hectares plus the mean winning-class probability - a cheap
#' whole-surface confidence figure that the original could never report
#' (finding 1.5).
#'
#' @param paths output of predict_site (class then prob)
#' @param site,tag ids
#' @return data.frame, one row per class present
summarise_prediction <- function(paths, site, tag) {
  cl <- terra::rast(paths[1])
  pr <- terra::rast(paths[2])
  px_ha <- prod(terra::res(cl)) / 1e4

  f <- terra::freq(cl)
  conf <- terra::app(pr, max, na.rm = TRUE)
  data.frame(
    site = site, tag = tag,
    Type = as.integer(f$value),
    n_pixels = f$count,
    area_ha = round(f$count * px_ha, 3),
    mean_top_prob = round(as.numeric(terra::global(conf, "mean", na.rm = TRUE)[1, 1]), 4),
    stringsAsFactors = FALSE
  )
}


#' Modal-smooth a predicted class surface
#'
#' The explicit version of what the original did implicitly: a w x w modal
#' focal filter over the hard classification (legacy Majority_filter.R used
#' w = 25 for drone surfaces). The manuscript describes a sieve filter that was
#' never implemented (finding 1.6); this pipeline runs the modal filter and
#' SAYS so. The raw surface is kept alongside, so the filter's effect on area
#' accounting is a measured quantity (7.31), not an assumption.
#'
#' NA cells (outside the AOI) stay NA: the smoothed surface is re-masked by the
#' raw one so the filter cannot grow the footprint at the margins.
#'
#' @param pred_paths output of predict_site (class path first)
#' @param window odd window size in cells; 0 or 1 returns the raw path
#' @param site,tag ids for the filename
#' @param out_dir output directory
#' @return path to the smoothed class raster
smooth_prediction <- function(pred_paths, window, site, tag,
                              out_dir = "data-out/predict") {
  class_path <- pred_paths[1]
  if (is.null(window) || window <= 1L) return(class_path)
  if (window %% 2L == 0L) {
    stop("smooth_window must be odd, got ", window, call. = FALSE)
  }
  r <- terra::rast(class_path)
  # Strip-processed, not a single focal() call: terra's C++ focal2 segfaults
  # ("memory not mapped") deterministically on bokspits_3's full raster - a
  # crash R cannot catch, which is why crew workers died silently on it. With a
  # half-window overlap per strip, every pixel still sees its complete
  # neighbourhood, so the output is IDENTICAL to a single pass (verified
  # against a banked single-pass surface; finding 7.31).
  half <- (window - 1L) %/% 2L
  strip_rows <- 4000L
  n <- terra::nrow(r)
  e <- terra::ext(r); rh <- (e$ymax - e$ymin) / n
  starts <- seq(1L, n, by = strip_rows)
  pieces <- lapply(starts, function(r0) {
    r1 <- min(r0 + strip_rows - 1L, n)
    pad0 <- max(1L, r0 - half); pad1 <- min(n, r1 + half)
    sub <- terra::crop(r, terra::ext(e$xmin, e$xmax,
                                     e$ymax - pad1 * rh, e$ymax - (pad0 - 1L) * rh))
    smp <- terra::focal(sub, w = window, fun = "modal", na.rm = TRUE)
    terra::crop(smp, terra::ext(e$xmin, e$xmax,
                                e$ymax - r1 * rh, e$ymax - (r0 - 1L) * rh))
  })
  sm <- if (length(pieces) == 1L) pieces[[1]] else do.call(terra::merge, pieces)
  sm <- terra::mask(sm, r)
  out <- file.path(out_dir, sub("_class\\.tif$",
                                sprintf("_class_smooth%d.tif", window),
                                basename(class_path)))
  terra::writeRaster(sm, out, overwrite = TRUE, datatype = "INT1U",
                     gdal = c("COMPRESS=LZW", "TILED=YES"), NAflag = 255)
  out
}


#' Class areas for any class raster
#'
#' @param class_tif path to a class raster
#' @param site,tag ids
#' @param surface label distinguishing raw from smoothed rows
#' @return data.frame, one row per class present
class_area_table <- function(class_tif, site, tag, surface) {
  cl <- terra::rast(class_tif)
  px_ha <- prod(terra::res(cl)) / 1e4
  f <- terra::freq(cl)
  data.frame(site = site, tag = tag, surface = surface,
             Type = as.integer(f$value), n_pixels = f$count,
             area_ha = round(f$count * px_ha, 3),
             stringsAsFactors = FALSE)
}


#' Raw vs smoothed area accounting
#'
#' The number that decides whether the smoothing debate matters: how much does
#' the modal filter move each class's area, per site?
#'
#' @param raw combined class_area_table rows for the raw surfaces
#' @param smoothed same for the smoothed surfaces
#' @return data.frame with per-class raw/smoothed areas and the delta
compare_areas <- function(raw, smoothed) {
  m <- merge(raw[, c("site", "Type", "area_ha")],
             smoothed[, c("site", "Type", "area_ha")],
             by = c("site", "Type"), all = TRUE, suffixes = c("_raw", "_smooth"))
  m$area_ha_raw[is.na(m$area_ha_raw)] <- 0
  m$area_ha_smooth[is.na(m$area_ha_smooth)] <- 0
  m$delta_ha <- round(m$area_ha_smooth - m$area_ha_raw, 3)
  m$delta_pct <- ifelse(m$area_ha_raw > 0,
                        round(100 * m$delta_ha / m$area_ha_raw, 1), NA)
  m[order(m$site, m$Type), ]
}
