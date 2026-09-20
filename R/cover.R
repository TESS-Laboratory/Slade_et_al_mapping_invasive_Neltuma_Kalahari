#' Sub-pixel Neltuma cover regression (C2) - the satellite arms' primary product
#'
#' Design (plan 3.8/3.9, finalized 2026-09-19 [HUGH]): the drone full
#' classification gives a per-pixel Neltuma probability; CALIBRATED and
#' area-averaged onto a coarser satellite grid it becomes the expected areal
#' Neltuma cover of each satellite cell - the regression target. No majority
#' vote, no purity threshold. Uncertainty is CV+ on the kNNDM folds, DI-stratified
#' (3.9); the aggregate area comes from PPI on top of the regression. This file
#' holds the drone-side calibration; the warp target, regression twins and the
#' DI/AOA machinery follow.

#' Calibrate the drone Neltuma probability so its mean equals areal cover
#'
#' The cover target is mean(P(Neltuma)) over a coarse cell, which equals the true
#' areal Neltuma fraction only if P is calibrated: among drone pixels the model
#' calls p, about a fraction p must truly be Neltuma. A soft-vote of tree learners
#' need not satisfy that. We fit a monotone recalibration g on the drone
#' OUT-OF-FOLD probabilities (already held out, so honest) against the binary
#' Neltuma truth, and apply g to the drone prediction surface before warping.
#'
#' Calibration fixes the PER-CELL target (and hence the cover map, the phases and
#' any AOA-restricted sub-region); it is distinct from PPI, which corrects the
#' scene AGGREGATE. Two calibrators are offered: "platt" (a two-parameter logistic
#' on the OOF logit - stable when the field truth is sparse) and "isotonic"
#' (nonparametric monotone - flexible but overfits the extremes with few points).
#' The caller keeps Platt unless the reliability curves clearly favour isotonic.
#'
#' @param sv soft-vote OOF `list(row_ids, prob = n x k, truth = factor)` from
#'   `softvote_oof()`, pooled across the drone sites for stability
#' @param neltuma_code Neltuma class code (column of `sv$prob`)
#' @param method "platt" or "isotonic"
#' @param eps clamp keeping the logit finite
#' @return a function g: [0, 1] -> [0, 1] with attributes "method" and (Platt) "coef"
calibrate_neltuma_prob <- function(sv, neltuma_code, method = c("platt", "isotonic"),
                                   eps = 1e-6) {
  method <- match.arg(method)
  nk <- as.character(neltuma_code)
  if (!nk %in% colnames(sv$prob)) {
    stop("Neltuma column '", nk, "' absent from the OOF probabilities.", call. = FALSE)
  }
  clamp <- function(x) pmin(pmax(as.numeric(x), eps), 1 - eps)
  p <- clamp(sv$prob[, nk])
  y <- as.integer(as.character(sv$truth) == nk)

  if (method == "platt") {
    fit <- stats::glm(y ~ stats::qlogis(p), family = stats::binomial())
    b <- unname(stats::coef(fit)[1L]); a <- unname(stats::coef(fit)[2L])
    g <- function(prob) stats::plogis(b + a * stats::qlogis(clamp(prob)))
    attr(g, "coef") <- c(intercept = b, slope = a)
  } else {
    ir <- stats::isoreg(p, y)
    xo <- ir$x[ir$ord]                       # x sorted ascending
    g <- function(prob) {
      stats::approx(xo, ir$yf, xout = clamp(prob), rule = 2, ties = "ordered")$y
    }
  }
  attr(g, "method") <- method
  g
}


#' Build the sub-pixel Neltuma cover target/training table for one drone site
#'
#' The warp-average, done as an area-weighted extraction. Mirrors
#' `build_training_table()` - same `exact_extract("mean")` feature extraction and
#' the same band-name contract - but the response is CONTINUOUS Neltuma cover, not
#' a class, and NO pixel is filtered: every satellite pixel over the drone site is
#' a training row (that is the whole point - no purity threshold). Cover is the
#' area-weighted mean of the CALIBRATED drone Neltuma probability over each
#' satellite pixel polygon; the nonlinear calibrator `g` is applied to the drone
#' raster BEFORE averaging, so the result is mean(g(p)), not g(mean(p)).
#'
#' @param cube_path satellite predictor cube (VRT)
#' @param prob_path drone probability raster for this site (GDAL-scaled to [0, 1])
#' @param grid_path satellite pixel-grid polygons over this drone site
#' @param site,tag ids recorded in the table
#' @param g calibrator from `calibrate_neltuma_prob()` (or any [0,1]->[0,1] fn)
#' @param nel_band band index of the Neltuma class in the drone prob raster
#' @return list(training = data.frame(cover, site, tag, x, y, <bands>),
#'   drops = attrition summary), rows with any NA feature or cover dropped
cover_training_table <- function(cube_path, prob_path, grid_path, site, tag, g, nel_band) {
  cube <- terra::rast(cube_path)
  grid <- sf::st_read(grid_path, quiet = TRUE)
  nel  <- terra::rast(prob_path)[[nel_band]]          # [0, 1] via the GDAL scale tag
  cov_r <- terra::app(nel, g)                          # calibrate BEFORE averaging

  cover <- exactextractr::exact_extract(cov_r, grid, "mean", progress = FALSE)
  ex <- exactextractr::exact_extract(cube, grid, "mean", progress = FALSE)
  if (is.null(dim(ex))) ex <- stats::setNames(data.frame(ex), paste0("mean.", names(cube)))
  ex <- as.data.frame(ex)
  names(ex) <- sub("^mean\\.", "", names(ex))
  if (!identical(names(ex), names(cube))) {
    stop("Cover feature columns do not match the cube bands for ", site, "/", tag,
         ".\n  cube: ", paste(names(cube), collapse = ", "),
         "\n  got : ", paste(names(ex), collapse = ", "), call. = FALSE)
  }

  xy <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(grid)))
  df <- data.frame(cover = as.numeric(cover), site = site, tag = tag,
                   x = xy[, 1], y = xy[, 2], ex, stringsAsFactors = FALSE)

  bands <- names(cube)
  ok <- stats::complete.cases(df[, c("cover", bands), drop = FALSE])
  drops <- data.frame(site = site, tag = tag, n_in = nrow(df), n_kept = sum(ok),
                      n_dropped = sum(!ok), stringsAsFactors = FALSE)
  list(training = df[ok, , drop = FALSE], drops = drops)
}


#' Dissimilarity index and area of applicability from the kNNDM folds (CAST)
#'
#' `aoa()` with our kNNDM CV folds, so the AOA threshold is the outlier-removed
#' maximum DI seen DURING cross-validation - the threshold is tied to the
#' prediction situation (3.9). Equal feature weights by default (`useWeight=FALSE`);
#' the permutation-importance variant passes `weight` (compared, simpler kept -
#' Q1). LPD is off (not core to the interval). The returned `di_cal` is the DI of
#' each training point (aligned to `train_df`'s row order, hence to the OOF
#' residuals) and feeds `di_conformal_bounds()`; `di_new` is the scene DI
#' (SpatRaster or vector) and `threshold` masks beyond the AOA.
#'
#' @param train_df cover training table (row order matches the OOF residuals)
#' @param bands predictor band names
#' @param folds kNNDM design `list(train_sets, test_sets)`
#' @param newdata scene cube (SpatRaster) or feature data.frame
#' @param weight optional 1-row data.frame of feature weights; NULL = equal
#' @return list(threshold, di_cal, di_new, aoa, raw)
cover_aoa <- function(train_df, bands, folds, newdata, weight = NULL) {
  args <- list(newdata = newdata, train = train_df[, bands, drop = FALSE],
               variables = bands, CVtrain = folds$train_sets,
               CVtest = folds$test_sets, useCV = TRUE, LPD = FALSE, verbose = FALSE)
  if (is.null(weight)) args$useWeight <- FALSE else args$weight <- weight
  a <- do.call(CAST::aoa, args)
  list(threshold = a$parameters$threshold,
       di_cal = as.numeric(a$parameters$trainDI),
       di_new = a$DI, aoa = a$AOA, raw = a)
}


#' Fast dissimilarity index + AOA threshold (KD-tree), scalable to full scenes
#'
#' `CAST::aoa` (see `cover_aoa()`) brute-forces the DI single-threaded and does
#' not scale to satellite scenes (S2's 4.5M px ran >30 min at 12 GB; WV2's 175M
#' px is infeasible). This computes the same quantity - Meyer & Pebesma's DI: the
#' nearest-training distance in scaled (equal-weight) feature space, normalised by
#' the mean pairwise training distance - via an FNN KD-tree, with the CV training
#' DI and the outlier-rule threshold (Q3 + 1.5*IQR) taken over the kNNDM folds.
#' `cover_aoa()` is retained for small-data cross-checks against CAST.
#'
#' @param train_df cover training table (row order matches the OOF residuals)
#' @param bands predictor band names
#' @param folds kNNDM (or leave-site-out) design, for the CV DI and threshold
#' @param weights optional per-band weights (permutation importance); NULL = equal
#' @param norm_sample number of point pairs sampled to estimate the normaliser
#' @return list(di_cal, threshold, di_of = function(feature matrix) -> DI vector,
#'   center, scale, weights, norm)
cover_di <- function(train_df, bands, folds, weights = NULL, norm_sample = 4000L) {
  X <- as.matrix(train_df[, bands, drop = FALSE])
  ctr <- colMeans(X); scl <- apply(X, 2, stats::sd); scl[scl == 0 | !is.finite(scl)] <- 1
  w <- if (is.null(weights)) rep(1, length(bands)) else as.numeric(weights)
  tr <- function(M) sweep(sweep(sweep(M, 2, ctr, "-"), 2, scl, "/"), 2, w, "*")
  Xs <- tr(X)

  set.seed(1L); m <- min(norm_sample, nrow(Xs))
  i1 <- sample(nrow(Xs), m, TRUE); i2 <- sample(nrow(Xs), m, TRUE)
  norm <- mean(sqrt(rowSums((Xs[i1, , drop = FALSE] - Xs[i2, , drop = FALSE])^2)))
  if (!is.finite(norm) || norm == 0) norm <- 1

  di_cal <- numeric(nrow(Xs))
  for (i in seq_along(folds$test_sets)) {
    te <- folds$test_sets[[i]]; trn <- folds$train_sets[[i]]
    di_cal[te] <- FNN::get.knnx(Xs[trn, , drop = FALSE], Xs[te, , drop = FALSE],
                                k = 1L)$nn.dist[, 1] / norm
  }
  threshold <- as.numeric(stats::quantile(di_cal, 0.75) + 1.5 * stats::IQR(di_cal))
  di_of <- function(M) FNN::get.knnx(Xs, tr(as.matrix(M)), k = 1L)$nn.dist[, 1] / norm
  list(di_cal = di_cal, threshold = threshold, di_of = di_of,
       center = ctr, scale = scl, weights = w, norm = norm)
}


#' DI-stratified (Mondrian) conformal cover intervals - the novel piece (3.9)
#'
#' Marginal conformal loses local coverage under covariate shift: the interval is
#' calibrated on the training feature distribution, so pixels far from training
#' (high dissimilarity index, DI) are under-covered. We stratify: bin the honest
#' kNNDM OOF residuals by DI (bins from the calibration DI quantiles, so each has
#' comparable support), take the split-conformal quantile of |residual| PER BIN,
#' and issue each scene pixel the quantile of its own DI bin. Intervals widen with
#' DI automatically. A bin too small to form the (1 - alpha) quantile gets an
#' infinite half-width (interval clamps to [0, 1] - maximal honesty, not a
#' fabricated tight bound). Beyond the AOA threshold there are no comparable
#' residuals at all, so no interval is issued (NA - the pixel is un-assessable).
#'
#' This is Mondrian conformal with DI as the stratifier, the direct analogue of
#' the per-class Mondrian in R/conformal.R. Coverage is EARNED empirically
#' (`di_coverage()` on held-out data), since kNNDM + DI-conditioning break the
#' exchangeability a finite-sample theorem would need.
#'
#' @param resid OOF residuals (truth - response) from `cover_ensemble_oof()`
#' @param di_cal DI of each calibration/OOF point (same order as `resid`)
#' @param di_new DI of each new (scene) point
#' @param yhat_new point cover prediction at each new point
#' @param alpha miscoverage level
#' @param n_bins number of DI strata (bins from calibration DI quantiles)
#' @param aoa_threshold DI above which no interval is issued (from `trainDI`)
#' @return data.frame(yhat, lower, upper, di, bin, inside_aoa) per new point,
#'   and attr(,"q") the per-bin half-widths
di_conformal_bounds <- function(resid, di_cal, di_new, yhat_new, alpha,
                                n_bins = 5L, aoa_threshold = Inf) {
  probs <- seq(0, 1, length.out = n_bins + 1L)
  edges <- unique(stats::quantile(di_cal, probs, na.rm = TRUE))
  edges[1] <- -Inf; edges[length(edges)] <- Inf
  nb <- length(edges) - 1L
  bin_cal <- findInterval(di_cal, edges, rightmost.closed = TRUE)
  bin_new <- findInterval(di_new, edges, rightmost.closed = TRUE)

  q_of <- function(r) {
    n <- length(r); if (n == 0L) return(NA_real_)
    k <- ceiling((n + 1) * (1 - alpha))
    if (k > n) return(Inf)                     # too few to guarantee -> maximal
    sort(abs(r))[k]
  }
  q <- vapply(seq_len(nb), function(b) q_of(resid[bin_cal == b]), numeric(1))

  qn <- q[bin_new]
  lo <- pmax(yhat_new - qn, 0); hi <- pmin(yhat_new + qn, 1)
  inside <- di_new <= aoa_threshold
  lo[!inside] <- NA_real_; hi[!inside] <- NA_real_
  out <- data.frame(yhat = yhat_new, lower = lo, upper = hi,
                    di = di_new, bin = bin_new, inside_aoa = inside)
  attr(out, "q") <- q
  out
}


#' Empirical coverage of cover intervals, overall and per DI bin
#'
#' The honesty check the guarantee is replaced by: on held-out data, does each DI
#' stratum actually cover ~ (1 - alpha)? This is the internal validation and,
#' later, the methods paper's headline figure.
#'
#' @param truth held-out cover values
#' @param lower,upper interval bounds (NA outside the AOA are dropped)
#' @param bin DI bin of each point
#' @return list(overall, by_bin = data.frame(bin, n, coverage))
di_coverage <- function(truth, lower, upper, bin) {
  ok <- !is.na(lower) & !is.na(upper)
  covered <- truth >= lower & truth <= upper
  by_bin <- do.call(rbind, lapply(sort(unique(bin[ok])), function(b) {
    idx <- ok & bin == b
    data.frame(bin = b, n = sum(idx), coverage = mean(covered[idx]))
  }))
  list(overall = mean(covered[ok]), by_bin = by_bin)
}


#' Spatial regression task for sub-pixel cover
#'
#' The regression twin of `make_task()`: coordinates drive spatial resampling
#' only (`coords_as_features = FALSE`), never predict. Response is the continuous
#' `cover` fraction.
#'
#' @param df a cover table from `cover_training_table()`
#' @param site,tag ids
#' @param epsg the unit CRS code
#' @return a TaskRegrST
make_cover_task <- function(df, site, tag, epsg) {
  drop <- intersect(c("site", "tag"), names(df))
  d <- df[, setdiff(names(df), drop), drop = FALSE]
  mlr3spatiotempcv::as_task_regr_st(
    d, target = "cover", id = paste0(site, "__", tag),
    coordinate_names = c("x", "y"), crs = paste0("EPSG:", epsg),
    coords_as_features = FALSE)
}


#' Build one regression twin of the classification learner set
#'
#' Same four capacity learners as the classification arm, in their regr guise,
#' each encapsulated with a featureless fallback so a failing spatial fold scores
#' as a failure rather than killing the run (as with_fallback() does for
#' classification).
#'
#' @param id "glmnet" | "ranger" | "lightgbm" | "svm"
#' @return a regr Learner
cover_learner <- function(id) {
  l <- switch(id,
    glmnet   = mlr3::lrn("regr.glmnet"),
    ranger   = mlr3::lrn("regr.ranger", importance = "impurity"),
    lightgbm = mlr3::lrn("regr.lightgbm", verbose = -1L, num_threads = 1L),
    svm      = mlr3::lrn("regr.svm", type = "eps-regression"),
    stop("Unknown cover learner '", id, "'.", call. = FALSE))
  l$encapsulate("evaluate", fallback = mlr3::lrn("regr.featureless"))
  l
}


#' Leave-one-site-out folds for the cover cells
#'
#' kNNDM's k-means clustering fails on the dense cover cells (thousands packed
#' into 7 sites); leave-one-site-out is the honest "predict an unseen site"
#' spatial CV for this design (finding 2026-09-20) and gives the residuals for the
#' DI-stratified conformal.
#'
#' @param train_df cover table with a `site` column
#' @return list(train_sets, test_sets) of row-index vectors, one fold per site
leave_site_out_folds <- function(train_df) {
  s <- factor(train_df$site); lv <- levels(s)
  list(train_sets = lapply(lv, function(l) which(s != l)),
       test_sets  = lapply(lv, function(l) which(s == l)))
}


#' Scene dissimilarity-index raster (block-processed via the FNN closure)
#'
#' Applies `cover_di()`'s `di_of` over the cube; terra handles blocking so it
#' scales past in-memory limits (WV2). Written as INT2S x1000 with a scale tag,
#' like the cover raster, so it reads back as DI.
#'
#' @param cube_path satellite cube
#' @param bands predictor band names
#' @param di_obj output of `cover_di()`
#' @param out_path output DI raster path
#' @param aoi optional study-area vector to crop/mask to
#' @return `out_path`
predict_di_raster <- function(cube_path, bands, di_obj, out_path, aoi = NULL) {
  cube <- terra::rast(cube_path)[[bands]]
  if (!is.null(aoi)) { v <- terra::vect(aoi); cube <- terra::mask(terra::crop(cube, v), v) }
  fun <- function(model, dat, ...) model$di_of(as.matrix(dat))
  n_cores <- as.integer(Sys.getenv("NELTUMA_PREDICT_CORES", "8"))
  di <- terra::predict(cube, di_obj, fun = fun, na.rm = FALSE, cores = n_cores)
  names(di) <- "di"
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(terra::round(di * 1000), out_path, overwrite = TRUE,
                     datatype = "INT2S", NAflag = -1L,
                     gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES"))
  ok <- system2("gdal_edit.py", c("-scale", "0.001", "-offset", "0", shQuote(out_path)),
                stdout = FALSE, stderr = FALSE)
  if (!identical(ok, 0L)) warning("gdal_edit.py did not tag the DI scale on ", out_path, call. = FALSE)
  out_path
}


#' Neltuma cover area from the scene surface: naive, within-AOA, and PPI-corrected
#'
#' Area = sum(cover x pixel) over the study area (naive) and over the AOA only
#' (label-supported). The within-AOA scene-MEAN cover is then PPI-corrected by the
#' model's bias measured on the labelled drone cells (rectifier = mean(OOF response
#' - true cover)); the CI carries the rectifier's own variance (drone-label
#' uncertainty enters here, once - plan 3.8). Out-of-AOA area is reported but
#' flagged as extrapolation.
#'
#' @param cover_path scene cover raster ([0,1] via scale tag)
#' @param di_path scene DI raster
#' @param threshold AOA DI threshold
#' @param oof ensemble OOF `list(response, truth)` on the drone cells
#' @param aoi study-area vector path
#' @param px_ha ha per pixel
#' @param sensor label
#' @param alpha CI level
#' @return one-row data.frame of areas
cover_scene_area <- function(cover_path, di_path, threshold, oof, aoi, px_ha,
                             sensor = NA_character_, alpha = 0.05) {
  v <- terra::vect(aoi)
  cover <- terra::mask(terra::rast(cover_path), v)
  di <- terra::mask(terra::rast(di_path), v)
  inside <- di <= threshold
  scene_cells  <- terra::global(!is.na(cover), "sum", na.rm = TRUE)[1, 1]
  inside_cells <- terra::global(inside, "sum", na.rm = TRUE)[1, 1]
  naive_ha  <- terra::global(cover, "sum", na.rm = TRUE)[1, 1] * px_ha
  aoa_ha    <- terra::global(terra::mask(cover, inside, maskvalue = FALSE), "sum", na.rm = TRUE)[1, 1] * px_ha
  theta_in  <- aoa_ha / (inside_cells * px_ha)              # within-AOA mean cover
  d <- oof$response - oof$truth
  delta <- mean(d); var_d <- stats::var(d) / length(d)
  theta_ppi <- min(max(theta_in - delta, 0), 1)
  se <- sqrt(theta_in * (1 - theta_in) / inside_cells + var_d)
  z <- stats::qnorm(1 - alpha / 2); tot_in <- inside_cells * px_ha
  data.frame(sensor = sensor, scene_ha = scene_cells * px_ha, aoa_ha = tot_in,
             aoa_frac = inside_cells / scene_cells, naive_ha = naive_ha,
             cover_aoa_ha = aoa_ha, ppi_ha = theta_ppi * tot_in,
             ppi_lo_ha = max(theta_ppi - z * se, 0) * tot_in,
             ppi_hi_ha = min(theta_ppi + z * se, 1) * tot_in,
             bias_pp = 100 * delta, n_overlap = length(d), stringsAsFactors = FALSE)
}


#' Fit the deployment cover ensemble on all data (plain regr task)
#'
#' The spatial (ST) task is only needed for the honest CV folds; the DEPLOYED
#' models train on a plain regr task over the bands, because coordinates are never
#' features and mlr3's ST `predict_newdata` rejects a bands-only prediction frame.
#' The fitted model is identical either way.
#'
#' @param train_df cover table
#' @param bands predictor band names
#' @param learner_ids regr twin ids
#' @return named list of trained regr Learners
fit_cover_models <- function(train_df, bands, learner_ids) {
  task <- mlr3::as_task_regr(train_df[, c("cover", bands), drop = FALSE],
                             target = "cover", id = "cover")
  stats::setNames(lapply(learner_ids, function(id) {
    l <- cover_learner(id); l$train(task); l
  }), learner_ids)
}


#' Resample one cover learner on a task's stored kNNDM design
#'
#' Mirrors `run_resample()`: the folds are the task's kNNDM design so residuals
#' are honest for spatial extrapolation and paired across learners.
#'
#' @param task a TaskRegrST
#' @param learner a regr Learner
#' @param folds `list(train_sets, test_sets)` (the kNNDM design)
#' @param seed RNG seed
#' @return a ResampleResult
run_cover_resample <- function(task, learner, folds, seed = 1L) {
  data.table::setDTthreads(1L)
  resampling <- as_custom_resampling(task, folds)
  set.seed(seed)
  mlr3::resample(task, learner, resampling, store_models = FALSE)
}


#' Out-of-fold cover predictions from a ResampleResult
#'
#' The regression analogue of `tidy_oof()`: one predicted cover per observation
#' (averaged over repeats), clamped to [0, 1] because cover is a fraction. These
#' are the honest kNNDM residual source for the DI-stratified conformal.
#'
#' @param rr a regr ResampleResult
#' @return list(row_ids, response, truth) aligned and sorted by row id
cover_oof <- function(rr) {
  p <- rr$prediction()
  resp <- pmin(pmax(p$response, 0), 1)
  ids <- p$row_ids; truth <- as.numeric(p$truth)
  uid <- sort(unique(ids))
  if (length(uid) < length(ids)) {
    resp <- as.numeric(tapply(resp, ids, mean)[as.character(uid)])
    truth <- truth[match(uid, ids)]
    ids <- uid
  } else {
    ord <- order(ids); resp <- resp[ord]; truth <- truth[ord]; ids <- ids[ord]
  }
  list(row_ids = ids, response = resp, truth = truth)
}


#' Equal-weight ensemble of cover OOF predictions
#'
#' The regression analogue of `softvote_oof()` and the D16 decision carried into
#' C2: equal weights are fixed a priori, so wrapping the conformal around this
#' averaged predictor stays honest (no nested selection leakage; plan 3.8).
#'
#' @param oof list of per-learner `cover_oof()` outputs (shared kNNDM folds)
#' @return list(row_ids, response, truth) for the averaged model
cover_ensemble_oof <- function(oof) {
  ids <- oof[[1]]$row_ids
  acc <- numeric(length(ids))
  for (o in oof) {
    ord <- match(ids, o$row_ids)
    if (anyNA(ord)) {
      stop("learners disagree on row ids; kNNDM folds should be shared.", call. = FALSE)
    }
    acc <- acc + o$response[ord]
  }
  list(row_ids = ids, response = acc / length(oof), truth = oof[[1]]$truth)
}


#' Pool the drone soft-vote OOF across sites and fit the Neltuma calibrator
#'
#' Pooled across the seven sites for stability (sparse field truth per site). The
#' drone OOF is already held out, so fitting the calibrator on it is honest.
#'
#' @param sv_list list of drone `softvote_oof()` outputs (one per site)
#' @param neltuma_code Neltuma class code
#' @param method "platt" or "isotonic"
#' @return calibrator g from `calibrate_neltuma_prob()`
drone_calibrator <- function(sv_list, neltuma_code, method = "platt") {
  nk <- as.character(neltuma_code)
  p <- unlist(lapply(sv_list, function(sv) as.numeric(sv$prob[, nk])), use.names = FALSE)
  y <- unlist(lapply(sv_list, function(sv) as.integer(as.character(sv$truth) == nk)),
              use.names = FALSE)
  pooled <- list(prob = matrix(p, ncol = 1L, dimnames = list(NULL, nk)),
                 truth = factor(ifelse(y == 1L, nk, "_rest"), levels = c("_rest", nk)))
  calibrate_neltuma_prob(pooled, neltuma_code, method = method)
}


#' Fit the cover ensemble on full data and predict the scene cover surface
#'
#' The regression analogue of `predict_unit_average()`: fit each regr twin on the
#' full cover table, predict the satellite cube, average the per-learner responses
#' (equal weight, clamped to [0, 1]) and write the cover raster.
#'
#' @param train_df cover table from `cover_training_table()`
#' @param cube_path satellite predictor cube
#' @param bands predictor band names
#' @param learner_ids regr twin ids
#' @param out_path output cover raster path
#' @param epsg CRS code for the task
#' @return `out_path`
predict_cover_scene <- function(train_df, cube_path, bands, learner_ids, out_path,
                                epsg = 32734) {
  data.table::setDTthreads(1L)
  models <- fit_cover_models(train_df, bands, learner_ids)

  cube <- terra::rast(cube_path)
  if (!all(bands %in% names(cube))) {
    stop("Cube lacks band(s): ", paste(setdiff(bands, names(cube)), collapse = ", "),
         call. = FALSE)
  }
  wrap <- function(model, dat, ...) {
    out <- rep(NA_real_, nrow(dat))
    ok <- stats::complete.cases(dat)
    if (any(ok)) {
      acc <- numeric(sum(ok))
      for (m in model) {
        acc <- acc + pmin(pmax(m$predict_newdata(dat[ok, , drop = FALSE])$response, 0), 1)
      }
      out[ok] <- acc / length(model)
    }
    out
  }
  n_cores <- as.integer(Sys.getenv("NELTUMA_PREDICT_CORES", "8"))
  cover <- terra::predict(cube[[bands]], models, fun = wrap, na.rm = FALSE, cores = n_cores)
  names(cover) <- "cover"
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(terra::round(cover * PROB_SCALE), out_path, overwrite = TRUE,
                     datatype = "INT2S", NAflag = -1L,
                     gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES"))
  tag_prob_scale(out_path)
  out_path
}


#' Reliability of a probability against a binary outcome
#'
#' Expected calibration error (bin-weighted |mean(p) - mean(y)|) and Brier score,
#' the two numbers that decide Platt vs isotonic and that the coverage figures
#' report. Empty bins are dropped.
#'
#' @param prob predicted probabilities on [0, 1]
#' @param y 0/1 outcomes
#' @param bins number of equal-width probability bins
#' @return list(ece, brier, n): scalars and the sample size
calibration_quality <- function(prob, y, bins = 10L) {
  prob <- as.numeric(prob); y <- as.numeric(y)
  br <- mean((prob - y)^2)
  cut <- findInterval(prob, seq(0, 1, length.out = bins + 1L), rightmost.closed = TRUE)
  ece <- 0
  for (b in unique(cut)) {
    in_b <- cut == b
    ece <- ece + mean(in_b) * abs(mean(prob[in_b]) - mean(y[in_b]))
  }
  list(ece = ece, brier = br, n = length(prob))
}
