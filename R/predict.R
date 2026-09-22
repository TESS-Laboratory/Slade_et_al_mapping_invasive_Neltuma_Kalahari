#' Landscape prediction
#'
#' One classified surface per unit: the equal-weight average of the tuned
#' learners' class probabilities (D16), each refitted on all of the unit's
#' training data with its tuned configuration, predicted over the cube through
#' the tiled single-copy engine in R/cover.R.
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

#' Probability rasters are stored as Int16 x PROB_SCALE, with a GDAL Scale tag
#' (1/PROB_SCALE) written into every band by `tag_prob_scale()`. terra and any
#' GDAL-scale-aware reader therefore return [0, 1] automatically, so read_prob()
#' is now a plain read - the +/- 10000 convention lives in the file metadata,
#' not in this code (decision 2026-09-18 [HUGH]).
PROB_SCALE <- 10000L

#' @param path a prob raster written by the prediction targets
#' @return SpatRaster of class probabilities on [0, 1]
read_prob <- function(path) terra::rast(path)

#' Embed the probability scale (1/PROB_SCALE) into a raster's band metadata
#'
#' terra's writeRaster cannot emit a GDAL Scale tag without also applying it and
#' destroying the integer values, so the tag is set as a post-write metadata
#' edit via gdal_edit. Self-describing: gdalinfo shows Scale=1e-04, and readers
#' that honour it (terra, gdalwarp, rasterio scaled reads) return [0, 1].
#'
#' @param path a written Int16 prob raster
#' @return `path`, invisibly
tag_prob_scale <- function(path) {
  scale <- format(1 / PROB_SCALE, scientific = FALSE)
  ok <- system2("gdal_edit.py", c("-scale", scale, "-offset", "0", shQuote(path)),
                stdout = FALSE, stderr = FALSE)
  if (!identical(ok, 0L)) {
    warning("gdal_edit.py did not tag the probability scale on ", path, call. = FALSE)
  }
  invisible(path)
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
  pr <- read_prob(paths[2])
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


#' Landscape prediction as an equal-weight average over the tuned learners
#'
#' Decision D16 (2026-09-17 [HUGH]; rationale in docs/refactor-3.0-plan.md
#' 3.5b). Finding 7.39: a single tuned model's hard map is one draw from a wide
#' distribution of equally "accurate" maps - the same learner retuned along a
#' different random path moved a site's Neltuma area by +54%. So there is no
#' winner: every tuned learner is refitted on all of the unit's training data
#' with its tuned configuration, and their class probabilities are averaged
#' with equal weights. No weights to tune, nothing to overfit, and no learner
#' is excluded as a "laggard" - that would be a qualitative call.
#'
#' ONE pass over the cube: each block is read once, predicted by every model,
#' averaged and written. v2.0's predictions were I/O-bound (load 104 at ~24
#' live cores), so five separate prediction runs would cost ~5x for nothing.
#'
#' Outputs: the averaged hard class (argmax), the averaged probabilities as
#' scaled integers, and one class layer per learner for the sensitivity table.
#'
#' @param cube_path predictor cube (VRT)
#' @param aoi_path boundary to mask to
#' @param training the unit's training table
#' @param specs named list of learner specs (tuned learners only)
#' @param shared evaluation settings (seed, predict_type)
#' @param configs named list of tuned configurations, same names as `specs`
#' @param site,tag ids for the filenames
#' @param aggregate fast-profile aggregation factor
#' @param out_dir output directory
#' @return c(class_path, prob_path, learners_path)
predict_unit_average <- function(cube_path, aoi_path, training, specs, shared, configs,
                                 site, tag, aggregate = 1L,
                                 out_dir = out_path("predict")) {
  data.table::setDTthreads(1L)
  stopifnot(length(specs) >= 1L, identical(sort(names(specs)), sort(names(configs))))

  feats <- setdiff(names(training), c("Type", "site", "tag", "x", "y"))
  task <- mlr3::as_task_classif(training[, c("Type", feats)], target = "Type",
                                id = paste0(site, "__", tag, "__final"))
  lvls <- task$class_names

  models <- lapply(names(specs), function(id) {
    learner <- bare_learner(specs[[id]], shared)
    cfg <- configs[[id]]
    if (!is.null(cfg)) {
      keep <- cfg[names(cfg) %in% learner$param_set$ids()]
      learner$param_set$set_values(.values = keep)
    }
    set.seed(shared$seed)
    learner$train(task)
    learner
  })
  names(models) <- names(specs)

  cube <- terra::rast(cube_path)
  if (aggregate > 1L) cube <- terra::aggregate(cube, fact = aggregate, fun = "mean", na.rm = FALSE)
  if (!all(feats %in% names(cube))) {
    stop("Cube ", basename(cube_path), " lacks feature band(s): ",
         paste(setdiff(feats, names(cube)), collapse = ", "), call. = FALSE)
  }
  cube <- cube[[feats]]

  # Tiled single-copy engine (R/cover.R), replacing terra::predict(cores = 8):
  # that path built a PSOCK cluster with five model copies per worker, no
  # resumability over a 4-8 h target, and ranger's per-call forest marshalling;
  # here one model copy predicts each row strip with ranger through the compiled
  # traversal (R/forest.R) and lightgbm/xgboost on their own thread pools, the
  # cube is cropped to the AOI first (S2 6x, Planet 1.4x fewer pixels) and
  # completed tiles survive a kill. Measured 2026-09-22.
  n_cores <- as.integer(Sys.getenv("NELTUMA_PREDICT_CORES", "8"))
  set_predict_threads(models, n_cores)
  preds <- lapply(models, fast_predictor, lvls = lvls, nthreads = n_cores)
  n_l <- length(models); n_c <- length(lvls); code <- as.integer(lvls)
  tile_fn <- function(v) {
    dat <- as.data.frame(v)
    acc <- matrix(0, nrow = nrow(v), ncol = n_c)
    out <- matrix(NA_real_, nrow = nrow(v), ncol = 1L + n_c + n_l)
    for (i in seq_len(n_l)) {
      pr <- preds[[i]](dat)
      acc <- acc + pr
      out[, 1L + n_c + i] <- code[max.col(pr, ties.method = "first")]
    }
    acc <- acc / n_l
    out[, 1L] <- code[max.col(acc, ties.method = "first")]
    out[, 1L + seq_len(n_c)] <- round(acc * PROB_SCALE)
    out
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  suffix <- if (aggregate > 1L) paste0("_agg", aggregate) else ""
  stem <- file.path(out_dir, paste0(site, "__", tag, suffix))
  class_path    <- paste0(stem, "_class.tif")
  prob_path     <- paste0(stem, "_prob.tif")
  learners_path <- paste0(stem, "_learners.tif")
  int_opts <- c("COMPRESS=LZW", "TILED=YES")
  writer <- function(vrt) {
    cl <- vrt[[1L]]; names(cl) <- "class"
    terra::writeRaster(cl, class_path, overwrite = TRUE, datatype = "INT1U", gdal = int_opts, NAflag = 255)
    pr <- vrt[[1L + seq_len(n_c)]]; names(pr) <- paste0("prob_", lvls)
    terra::writeRaster(pr, prob_path, overwrite = TRUE, datatype = "INT2S", NAflag = -1L,
                       gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES",
                                "BLOCKXSIZE=512", "BLOCKYSIZE=512"))
    lr <- vrt[[1L + n_c + seq_len(n_l)]]; names(lr) <- paste0("class_", names(models))
    terra::writeRaster(lr, learners_path, overwrite = TRUE, datatype = "INT1U", gdal = int_opts, NAflag = 255)
  }
  raster_predict_parallel(cube, aoi_path, class_path, tile_fn, nlyr = 1L + n_c + n_l,
                          engine = "threaded", writer = writer)
  tag_prob_scale(prob_path)
  c(class_path, prob_path, learners_path)
}


#' Per-learner class areas - the sensitivity table behind the average
#'
#' @param paths output of `predict_unit_average()`
#' @param site,tag ids
#' @return data.frame: site, tag, learner ("average" included), Type, area_ha
learner_area_table <- function(paths, site, tag) {
  avg <- terra::rast(paths[1]); per <- terra::rast(paths[3])
  px_ha <- prod(terra::res(avg)) / 1e4
  one <- function(r, name) {
    f <- terra::freq(r)
    data.frame(site = site, tag = tag, learner = name, Type = as.integer(f$value),
               area_ha = f$count * px_ha, stringsAsFactors = FALSE)
  }
  rows <- c(list(one(avg, "average")),
            lapply(names(per), function(n) one(per[[n]], sub("^class_", "", n))))
  do.call(rbind, rows)
}
