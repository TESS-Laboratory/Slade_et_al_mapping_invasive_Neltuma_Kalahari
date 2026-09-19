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
