#' Sub-pixel Neltuma cover regression (C2) - the satellite arms' primary product
#'
#' Design (plan 3.8/3.9, finalized 2026-09-19 [HUGH]): the drone full
#' classification gives a per-pixel Neltuma probability; CALIBRATED and
#' area-averaged onto a coarser satellite grid it becomes the expected areal
#' Neltuma cover of each satellite cell - the regression target. No majority
#' vote, no purity threshold. Uncertainty is DI-stratified split conformal on the
#' leave-one-site-out residuals (3.9), with coverage EARNED by a nested
#' leave-site-out check (cover_site_coverage); the aggregate area comes from a
#' cover-stratified PPI rectifier on top of the regression. This file holds the
#' drone-side calibration, the warp target, the regression twins and the DI/AOA
#' machinery.

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
  attr(out, "edges") <- edges
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
  # Thread the FIT (large cover training sets: ~150k rows Planet, ~690k WV2).
  # Fitting runs in one crew task at a time, so N threads is safe; the mirai
  # daemons override each model to 1 thread at PREDICT so N daemons = N cores,
  # no oversubscription (raster_predict_parallel).
  nthr <- as.integer(Sys.getenv("NELTUMA_PREDICT_CORES", "8"))
  l <- switch(id,
    glmnet   = mlr3::lrn("regr.glmnet"),
    ranger   = mlr3::lrn("regr.ranger", importance = "impurity", num.threads = nthr),
    lightgbm = mlr3::lrn("regr.lightgbm", verbose = -1L, num_threads = nthr),
    svm      = mlr3::lrn("regr.svm", type = "eps-regression"),
    stop("Unknown cover learner '", id, "'.", call. = FALSE))
  l$encapsulate("evaluate", fallback = mlr3::lrn("regr.featureless"))
  l
}


PHASE_LABELS <- c("Pre-Incursion", "Initial Incursion", "Expansion", "Dominance")

#' Per-DI-bin conformal half-widths as a raster reclassification table
#'
#' @param oof ensemble OOF; @param di_obj `cover_di()` output; @param alpha level
#' @return matrix(from, to, q) for `terra::classify()` on a DI raster
cover_halfwidth_rcl <- function(oof, di_obj, alpha = 0.10, n_bins = 5L) {
  yhat <- pmin(pmax(oof$response, 0), 1)
  b <- di_conformal_bounds(oof$truth - oof$response, di_obj$di_cal, di_obj$di_cal, yhat,
                           alpha, n_bins = n_bins, aoa_threshold = Inf)
  e <- attr(b, "edges"); q <- attr(b, "q")
  q[!is.finite(q)] <- 1
  cbind(from = e[-length(e)], to = e[-1], q = q)
}


#' Invasion phase per grid cell from the continuous cover surface, with its interval
#'
#' The cover-based replacement for `build_phase_layer()`: mean of the CONTINUOUS
#' cover surface per hexagon (no hard-class majority, no modal filter), assigned to
#' a phase by the sensors.yml thresholds. Revised 2026-09-22 (D7, R1 L272): the
#' hexagon mean is also taken over the LOWER and UPPER 90% conformal bounds of its
#' pixels (half-width by DI stratum, `cover_halfwidth_rcl`), and each is assigned a
#' phase too, so Table 1 carries a range. Only pixels within the AOA contribute to
#' the assessed cover; a hexagon with less than half its pixels inside the AOA gets
#' no phase and is reported as "beyond applicability". `cover_pct` (all pixels) is
#' kept for the un-masked map.
#'
#' NOTE on semantics (finding 2026-09-22): the Table S8 thresholds were defined for
#' the share of a cell classified as Neltuma; here they cut MEAN SUB-PIXEL COVER.
#' A regression never predicts exactly zero, so the 0.1% pre-incursion floor is
#' unattainable for a hexagon mean and the model's own detection floor
#' (`cover_error_table`, ~2%) sits at the incursion/expansion boundary. The point
#' phase is therefore reported together with its interval range and the below-floor
#' share (`cover_phase_summary`), never alone.
#'
#' @param cover_path scene cover raster ([0,1] via scale tag)
#' @param di_path scene DI raster ([DI] via scale tag)
#' @param threshold AOA DI threshold
#' @param oof,di_obj ensemble OOF and `cover_di()` output (for the half-widths)
#' @param grid_path the analysis grid (250 m hexagons)
#' @param aoi study-area vector path
#' @param phases sensors.yml phases block (incursion/expansion/dominance, in %)
#' @param out_path output layer path (.fgb)
#' @param alpha interval level for the range
#' @return `out_path`
cover_phase_layer <- function(cover_path, di_path, threshold, oof, di_obj, grid_path, aoi,
                              phases, out_path, alpha = 0.10) {
  cover <- terra::rast(cover_path); di <- terra::rast(di_path)
  grid  <- sf::st_read(grid_path, quiet = TRUE)
  av    <- sf::st_union(sf::st_read(aoi, quiet = TRUE))
  grid  <- grid[lengths(sf::st_intersects(grid, av)) > 0, ]
  inside <- di <= threshold
  q <- terra::classify(di, cover_halfwidth_rcl(oof, di_obj, alpha), include.lowest = TRUE, right = TRUE)
  cov_in <- terra::mask(cover, inside, maskvalues = c(0, NA))
  lower <- terra::clamp(cov_in - q, 0, 1); upper <- terra::clamp(cov_in + q, 0, 1)
  stack <- c(cover, inside, cov_in, lower, upper)
  names(stack) <- c("cover_pct", "aoa_frac", "cover_aoa_pct", "lower_pct", "upper_pct")
  ex <- exactextractr::exact_extract(stack, grid, "mean", progress = FALSE)
  names(ex) <- sub("^mean\\.", "", names(ex))
  for (v in c("cover_pct", "cover_aoa_pct", "lower_pct", "upper_pct")) grid[[v]] <- 100 * ex[[v]]
  grid$aoa_frac <- ex$aoa_frac
  assessed <- !is.na(grid$aoa_frac) & grid$aoa_frac >= 0.5
  phase_of <- function(p) as.character(cut(p, breaks = c(-Inf, phases$incursion, phases$expansion, phases$dominance, Inf),
                                           labels = PHASE_LABELS, right = FALSE))
  grid$phase <- ifelse(assessed, phase_of(grid$cover_aoa_pct), NA_character_)
  grid$phase_lower <- ifelse(assessed, phase_of(grid$lower_pct), NA_character_)
  grid$phase_upper <- ifelse(assessed, phase_of(grid$upper_pct), NA_character_)
  write_fgb(grid, out_path)
  out_path
}


#' Area per invasion phase from a cover phase layer, with the interval range
#'
#' @param layer_path output of `cover_phase_layer()`
#' @param sensor label
#' @param floor_pct the model's detection floor (% cover); NA to skip
#' @return data.frame(sensor, phase, area_ha, pct_of_area, pct_lower, pct_upper,
#'   n_cells) over the fixed phase levels, plus a "Beyond applicability" row and,
#'   when `floor_pct` is given, a "Below detection floor" row (assessed hexagons
#'   whose cover is under the floor; overlaps the phase rows). Percentages are of
#'   the total hexagon area.
cover_phase_summary <- function(layer_path, sensor = NA_character_, floor_pct = NA_real_) {
  g <- sf::st_read(layer_path, quiet = TRUE)
  a <- as.numeric(sf::st_area(g)) / 1e4
  tot <- sum(a)
  share <- function(idx) if (tot > 0) 100 * sum(a[idx]) / tot else 0
  assessed <- !is.na(g$phase)
  rows <- lapply(PHASE_LABELS, function(p) {
    idx <- assessed & g$phase == p
    data.frame(sensor = sensor, phase = p, area_ha = sum(a[idx]), pct_of_area = share(idx),
               pct_lower = share(assessed & g$phase_lower == p),
               pct_upper = share(assessed & g$phase_upper == p),
               n_cells = sum(idx), stringsAsFactors = FALSE)
  })
  rows[[length(rows) + 1L]] <- data.frame(sensor = sensor, phase = "Beyond applicability",
    area_ha = sum(a[!assessed]), pct_of_area = share(!assessed), pct_lower = share(!assessed),
    pct_upper = share(!assessed), n_cells = sum(!assessed), stringsAsFactors = FALSE)
  if (is.finite(floor_pct)) {
    idx <- assessed & g$cover_aoa_pct < floor_pct
    rows[[length(rows) + 1L]] <- data.frame(sensor = sensor, phase = "Below detection floor",
      area_ha = sum(a[idx]), pct_of_area = share(idx),
      pct_lower = share(assessed & g$lower_pct < floor_pct), pct_upper = share(assessed & g$upper_pct < floor_pct),
      n_cells = sum(idx), stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}


#' Mean hexagon cover by distance from roads and settlements (descriptive, R1 L278)
#'
#' Replaces the lost Figures S10-S11 with a pipeline product: mean predicted cover
#' of the AOA-supported hexagons (`cover_phase_layer`) in distance bands from the
#' nearest OSM road and the nearest village. Descriptive only - no inference -
#' consistent with the authors' response to Reviewer 1.
#'
#' @param layer_path output of `cover_phase_layer()`
#' @param roads_path,setts_path OSM vectors (.fgb)
#' @param sensor label
#' @param road_breaks,sett_breaks band edges in metres
#' @return long data.frame(sensor, feature, band, n, mean_cover_pct, median_cover_pct)
cover_gradient_table <- function(layer_path, roads_path, setts_path, sensor = NA_character_,
                                 road_breaks = c(0, 250, 1000, 3000, Inf),
                                 sett_breaks = c(0, 1000, 3000, 6000, Inf)) {
  g <- sf::st_read(layer_path, quiet = TRUE)
  g <- g[!is.na(g$phase), ]
  cen <- sf::st_centroid(sf::st_geometry(g))
  dist_to <- function(path) {
    v <- sf::st_transform(sf::st_read(path, quiet = TRUE), sf::st_crs(g))
    as.numeric(sf::st_distance(cen, sf::st_union(sf::st_geometry(v))))[seq_along(cen)]
  }
  lab <- function(br) {
    lo <- br[-length(br)]; hi <- br[-1]
    ifelse(is.infinite(hi), paste0(">", lo / 1000, " km"),
           ifelse(hi < 1000, paste0(lo, "-", hi, " m"), paste0(lo / 1000, "-", hi / 1000, " km")))
  }
  one <- function(d, br, feature) {
    band <- cut(d, br, labels = lab(br), right = FALSE, include.lowest = TRUE)
    do.call(rbind, lapply(levels(band), function(b) {
      i <- !is.na(band) & band == b
      data.frame(sensor = sensor, feature = feature, band = b, n = sum(i),
                 mean_cover_pct = if (any(i)) mean(g$cover_aoa_pct[i]) else NA_real_,
                 median_cover_pct = if (any(i)) stats::median(g$cover_aoa_pct[i]) else NA_real_,
                 stringsAsFactors = FALSE)
    }))
  }
  rbind(one(dist_to(roads_path), road_breaks, "road"),
        one(dist_to(setts_path), sett_breaks, "settlement"))
}


#' Predictor band columns of a cover table (everything but the metadata)
#' @param train_df cover table
#' @return character band names
cover_bands <- function(train_df) {
  setdiff(names(train_df), c("cover", "site", "tag", "x", "y"))
}


#' Run the cover regression twins on the folds and return the ensemble OOF
#'
#' @param train_df cover table
#' @param ids regr twin ids (svm dropped: worst + slowest, finding 2026-09-20)
#' @param folds leave-site-out (or kNNDM) design
#' @param epsg CRS code
#' @return ensemble OOF `list(row_ids, response, truth)`
cover_run_oof <- function(train_df, ids, folds, epsg = 32734) {
  task <- make_cover_task(train_df, train_df$site[1], train_df$tag[1], epsg)
  oof <- lapply(ids, function(id) cover_oof(run_cover_resample(task, cover_learner(id), folds)))
  cover_ensemble_oof(oof)
}


#' Per-cell honest (nested leave-site-out) conformal coverage
#'
#' The apparent coverage of split conformal - quantiles fit on the OOF residuals
#' and evaluated on the same residuals - lands on the nominal level by
#' construction (finding 2026-09-22: 0.900/0.901/0.908 at 90% for WV2/Planet/S2,
#' exactly ceil((n+1)(1-a))/n). It is not evidence that the intervals transfer to
#' an unseen site. This nests the calibration: for each site in turn the per-bin
#' quantiles are fit on the OTHER sites' residuals (their own DI bins) and applied
#' to the held-out site, so every cell's interval was calibrated without it. The
#' point predictions are already leave-site-out (`cover_run_oof`), so the result
#' is the coverage a new survey area would see.
#'
#' @param oof ensemble OOF `list(row_ids, response, truth)`
#' @param di_cal DI of each OOF cell (own site excluded, from `cover_di()`)
#' @param site site of each OOF cell (aligned to `oof$row_ids`)
#' @param alpha miscoverage level
#' @param n_bins conformal DI strata (must match the deployed product)
#' @return list(covered = logical per cell, width = numeric per cell)
cover_nested_coverage <- function(oof, di_cal, site, alpha, n_bins = 5L) {
  resid <- oof$truth - oof$response
  yhat <- pmin(pmax(oof$response, 0), 1)
  covered <- logical(length(resid)); width <- numeric(length(resid))
  for (k in unique(site)) {
    cal <- site != k; te <- site == k
    b <- di_conformal_bounds(resid[cal], di_cal[cal], di_cal[te], yhat[te], alpha,
                             n_bins = n_bins, aoa_threshold = Inf)
    covered[te] <- oof$truth[te] >= b$lower & oof$truth[te] <= b$upper
    width[te] <- b$upper - b$lower
  }
  list(covered = covered, width = width)
}


#' DI-stratified conformal coverage of the ensemble OOF across alphas
#'
#' Reports BOTH the apparent coverage (quantiles fit and evaluated on the same
#' residuals; equals the nominal rate by construction and is shown only as the
#' reference) and the honest nested leave-site-out coverage of
#' `cover_nested_coverage()`, within the deployed AOA threshold. The honest column
#' is the number the paper reports.
#'
#' @param oof ensemble OOF
#' @param di_obj `cover_di()` output
#' @param alphas miscoverage levels
#' @param sensor label
#' @param threshold AOA DI threshold (cells beyond it carry no interval)
#' @param train_df cover table (its `site` column, aligned to `oof$row_ids`)
#' @return data.frame(sensor, alpha, nominal, apparent, honest, mean_width, n, n_inside)
cover_coverage_table <- function(oof, di_obj, alphas, sensor = NA_character_,
                                 threshold = di_obj$threshold, train_df = NULL) {
  resid <- oof$truth - oof$response
  inside <- di_obj$di_cal <= threshold
  site <- if (is.null(train_df)) rep("all", length(resid)) else as.character(train_df$site[oof$row_ids])
  do.call(rbind, lapply(alphas, function(a) {
    b <- di_conformal_bounds(resid, di_obj$di_cal, di_obj$di_cal, oof$response, a, 5L, threshold)
    app <- di_coverage(oof$truth, b$lower, b$upper, b$bin)$overall
    hon <- if (length(unique(site)) > 1L) cover_nested_coverage(oof, di_obj$di_cal, site, a) else
      list(covered = oof$truth >= b$lower & oof$truth <= b$upper, width = b$upper - b$lower)
    data.frame(sensor = sensor, alpha = a, nominal = 1 - a,
               apparent = app, honest = mean(hon$covered[inside]),
               mean_width = mean(hon$width[inside]),
               n = length(oof$truth), n_inside = sum(inside), stringsAsFactors = FALSE)
  }))
}


#' Honest coverage per survey area (the site table the paper reports)
#'
#' @inheritParams cover_coverage_table
#' @return data.frame(sensor, site, alpha, n, n_inside, frac_inside, coverage, width)
cover_site_coverage <- function(oof, di_obj, alphas, sensor = NA_character_,
                                threshold = di_obj$threshold, train_df) {
  site <- as.character(train_df$site[oof$row_ids])
  inside <- di_obj$di_cal <= threshold
  do.call(rbind, lapply(alphas, function(a) {
    hon <- cover_nested_coverage(oof, di_obj$di_cal, site, a)
    do.call(rbind, lapply(sort(unique(site)), function(k) {
      i <- site == k; j <- i & inside
      data.frame(sensor = sensor, site = k, alpha = a, n = sum(i), n_inside = sum(j),
                 frac_inside = sum(j) / sum(i),
                 coverage = if (any(j)) mean(hon$covered[j]) else NA_real_,
                 width = if (any(j)) mean(hon$width[j]) else NA_real_, stringsAsFactors = FALSE)
    }))
  }))
}


#' Coverage-driven AOA threshold (replaces the Q3+1.5*IQR fence)
#'
#' The Tukey fence on the training DI (`cover_di`) is set by the sparse dune matrix
#' that dominates the drone-overlap cells, so it excludes the DENSE-cover regime -
#' the river/road corridors where Neltuma is actually concentrated - even though we
#' hold thousands of dense training cells there (finding 2026-09-22 [HUGH]: ~15-39%
#' of predicted cover, and ~48% of the >25%-cover training cells, fell BEYOND the
#' fence, driving a large impact underestimate). Instead we tie the AOA directly to
#' the guarantee we can keep: bin the HONEST (nested leave-site-out) per-cell
#' coverage by DI, and extend the AOA up to the last DI band whose coverage still
#' meets `coverage_floor`. Beyond that the intervals genuinely under-cover and the
#' cell is excluded; within it the corridors are (rightly) included.
#'
#' Revised 2026-09-22 after the statistical review: (i) the coverage used to be the
#' APPARENT coverage, which sits on the nominal level in every band by construction
#' and so never stopped the walk before the cap; (ii) the walk required contiguous
#' passing bands from the lowest DI up, so one noisy low-DI band (honest coverage
#' 0.842 vs a 0.85 floor for WV2) discarded the whole procedure. Honest coverage is
#' flat at ~0.87-0.92 across the training bulk for every sensor and collapses only
#' in the top band, which is dominated by the single dense site, so the rule is now
#' "the last band whose honest coverage meets the floor", capped at `cap_quantile`
#' of the training DI. The floor is a stated tolerance below nominal (0.85 at
#' alpha = 0.10), not a guarantee.
#'
#' @param oof ensemble OOF list(row_ids, response, truth)
#' @param di_obj output of `cover_di()` (its `di_cal`, and `threshold` as fallback)
#' @param train_df cover table (its `site` column); NULL falls back to apparent
#'   coverage (single-site smoke runs only)
#' @param alpha miscoverage level the floor is judged at (default the middle 0.10)
#' @param coverage_floor minimum honest coverage to keep including a DI band
#' @param curve_bins equal-count DI bands (within the cap) for the coverage curve
#' @param conf_bins conformal DI bins (must match the deployed `cover_coverage_table`)
#' @param cap_quantile do not extend the AOA past this quantile of training DI - the
#'   heavy DI tail (a few spectrally extreme training cells) is genuine outlier
#'   territory, and quantile bands there are too sparse to judge coverage reliably
#' @return scalar DI threshold, with attributes "rule" ("coverage", "cap" or
#'   "fence"), "last_good" (band index) and "curve" (band coverage table)
cover_aoa_threshold <- function(oof, di_obj, train_df = NULL, alpha = 0.10,
                                coverage_floor = 0.85, curve_bins = 10L, conf_bins = 5L,
                                cap_quantile = 0.99) {
  di_cal <- di_obj$di_cal
  site <- if (is.null(train_df)) NULL else as.character(train_df$site[oof$row_ids])
  cov_ok <- if (!is.null(site) && length(unique(site)) > 1L) {
    cover_nested_coverage(oof, di_cal, site, alpha, n_bins = conf_bins)$covered
  } else {
    yhat <- pmin(pmax(oof$response, 0), 1)
    b <- di_conformal_bounds(oof$truth - oof$response, di_cal, di_cal, yhat, alpha,
                             n_bins = conf_bins, aoa_threshold = Inf)
    oof$truth >= b$lower & oof$truth <= b$upper
  }
  cap <- as.numeric(stats::quantile(di_cal, cap_quantile, na.rm = TRUE))
  sel <- di_cal <= cap; di_s <- di_cal[sel]; ok_s <- cov_ok[sel]
  edges <- unique(stats::quantile(di_s, seq(0, 1, length.out = curve_bins + 1L), na.rm = TRUE))
  edges[1] <- -Inf; nb <- length(edges) - 1L
  band <- findInterval(di_s, edges, rightmost.closed = TRUE)
  curve <- data.frame(band = seq_len(nb), di_hi = edges[-1],
                      n = as.numeric(table(factor(band, seq_len(nb)))),
                      coverage = as.numeric(tapply(ok_s, factor(band, seq_len(nb)), mean)))
  good <- which(!is.na(curve$coverage) & curve$coverage >= coverage_floor)
  if (!length(good)) {
    thr <- as.numeric(di_obj$threshold); rule <- "fence"; last_good <- 0L
  } else {
    last_good <- max(good)
    thr <- edges[last_good + 1L]
    rule <- if (last_good == nb) "cap" else "coverage"
    if (!is.finite(thr)) thr <- cap
    thr <- as.numeric(min(thr, cap))
  }
  attr(thr, "rule") <- rule; attr(thr, "last_good") <- last_good; attr(thr, "curve") <- curve
  thr
}


#' Held-out error of the cover ensemble per survey area
#'
#' Leave-site-out RMSE, bias, R^2 and mean cover per site plus a pooled row, and
#' the model's detection floor: the mean OOF prediction over cells whose drone
#' cover is below `floor_truth` (0.1%, the Table S8 incursion threshold). The floor
#' is the predicted cover a Neltuma-free cell receives, and it bounds what the
#' phase thresholds can resolve (finding 2026-09-22: 1.7 / 2.8 / 2.1% for
#' WV2/Planet/S2, above the 1.5% expansion threshold for two sensors).
#'
#' @param oof ensemble OOF; @param train_df cover table; @param sensor label
#' @param floor_truth truth cover below which a cell counts as Neltuma-free
#' @return data.frame(sensor, site, n, mean_truth_pct, mean_pred_pct, rmse_pp,
#'   bias_pp, r2, noise_floor_pct, n_floor)
cover_error_table <- function(oof, train_df, sensor = NA_character_, floor_truth = 0.001) {
  site <- as.character(train_df$site[oof$row_ids])
  yhat <- pmin(pmax(oof$response, 0), 1); truth <- oof$truth
  one <- function(i, label) {
    z <- i & truth < floor_truth
    data.frame(sensor = sensor, site = label, n = sum(i),
               mean_truth_pct = 100 * mean(truth[i]), mean_pred_pct = 100 * mean(yhat[i]),
               rmse_pp = 100 * sqrt(mean((yhat[i] - truth[i])^2)),
               bias_pp = 100 * mean(yhat[i] - truth[i]),
               r2 = if (sum(i) > 2) stats::cor(yhat[i], truth[i])^2 else NA_real_,
               noise_floor_pct = if (any(z)) 100 * mean(yhat[z]) else NA_real_,
               n_floor = sum(z), stringsAsFactors = FALSE)
  }
  rbind(one(rep(TRUE, length(truth)), "pooled"),
        do.call(rbind, lapply(sort(unique(site)), function(k) one(site == k, k))))
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


#' Tiled, resumable scene prediction (threaded single-copy engine, or mirai daemons)
#'
#' History: terra::predict's own cluster, nested inside a crew daemon, re-serialised
#' the heavy models per block and stalled (2026-09-20 [HUGH]); mirai daemons fixed
#' that but copied the ~28 GB WV2 cover forest to every daemon and OOM-killed the
#' box (2026-09-21); the single-copy THREADED engine keeps one model in-process and
#' parallelises each tile across rows (ranger via the compiled traversal in
#' R/forest.R, lightgbm/xgboost via their own thread pools). Generalised 2026-09-22
#' (prediction-arm review): the engine now takes a `tile_fn` so the hard-class
#' 5-learner average, the cover ensemble and the DI raster share one code path,
#' one crop-to-AOI, one resumable atomic tiling and one write path.
#'
#' Tiles are row strips of ~`tile_cells` cells (`makeTiles` is deterministic, so a
#' restart maps to the same in_N/out_N files); tiling is skipped when the expected
#' input tiles already exist; each output tile is written atomically (temp +
#' rename) so a kill can never leave a truncated tile a later resume would trust.
#'
#' @param cube SpatRaster of predictor bands (already band-subset, names set)
#' @param aoi study-area vector path to crop/mask to, or NULL
#' @param out_path final raster path (see `writer`)
#' @param tile_fn function(v) -> numeric matrix (nrow(v) x nlyr) of INTEGER-valued
#'   outputs for the complete-case rows `v` (a matrix with band-name columns);
#'   the caller applies its own scale (e.g. x 10000) inside `tile_fn`
#' @param nlyr number of output layers `tile_fn` returns
#' @param n number of daemons (daemon engine) / threads hint (threaded engine)
#' @param tile_cells cells per tile
#' @param engine "threaded" (one model copy in-process; default) or "daemon"
#'   (`tile_fn` and its environment are sent to `n` mirai daemons on the
#'   "coverpred" profile - only for small closures such as the DI KD-tree)
#' @param writer function(vrt SpatRaster) writing the final product(s); default
#'   writes a single INT2S mosaic to `out_path`
#' @param packages packages the daemon engine attaches
#' @return `out_path`
raster_predict_parallel <- function(cube, aoi, out_path, tile_fn, nlyr = 1L,
                                    n = as.integer(Sys.getenv("NELTUMA_PREDICT_CORES", "8")),
                                    tile_cells = as.numeric(Sys.getenv("NELTUMA_TILE_CELLS", "2e6")),
                                    engine = c("threaded", "daemon"),
                                    writer = NULL,
                                    packages = c("terra", "FNN")) {
  engine <- match.arg(engine)
  if (!is.null(aoi)) { v <- terra::vect(aoi); cube <- terra::mask(terra::crop(cube, v), v) }
  bands <- names(cube)
  tdir <- paste0(out_path, ".tiles")
  nrpt <- max(1L, as.integer(ceiling(tile_cells / terra::ncol(cube))))
  # Resume only against tiles of THIS geometry: a tile directory left by a run of a
  # different cube (another profile's aggregation, a different band set or nlyr) is
  # wiped, not reused (2026-09-22: a killed run's full-resolution tiles were mosaicked
  # into an aggregated product).
  key <- paste(paste(dim(cube), collapse = "x"), paste(signif(as.vector(terra::ext(cube)), 12), collapse = ","),
               paste(signif(terra::res(cube), 12), collapse = ","), nlyr, nrpt, paste(bands, collapse = "|"))
  keyfile <- file.path(tdir, "geometry.key")
  if (dir.exists(tdir) && !(file.exists(keyfile) && identical(readLines(keyfile, warn = FALSE), key))) {
    unlink(tdir, recursive = TRUE)
  }
  dir.create(tdir, recursive = TRUE, showWarnings = FALSE)
  writeLines(key, keyfile)
  n_tiles <- as.integer(ceiling(terra::nrow(cube) / nrpt))
  expected <- file.path(tdir, sprintf("in_%d.tif", seq_len(n_tiles)))
  intiles <- if (all(file.exists(expected))) expected else
    terra::makeTiles(cube, c(nrpt, terra::ncol(cube)), file.path(tdir, "in_.tif"),
                     na.rm = FALSE, overwrite = TRUE)
  outtiles <- sub("in_", "out_", intiles, fixed = TRUE)
  todo <- intiles[!file.exists(outtiles)]                     # resume: skip completed tiles

  one_tile <- function(tp, fn, bands, nlyr) {
    r <- terra::rast(tp); names(r) <- bands                   # makeTiles may drop names
    v <- terra::values(r, mat = TRUE)
    out <- matrix(NA_real_, nrow = nrow(v), ncol = nlyr)
    ok <- stats::complete.cases(v)
    if (any(ok)) out[ok, ] <- fn(v[ok, , drop = FALSE])
    o <- terra::rast(r, nlyrs = nlyr)
    terra::values(o) <- as.integer(round(out))
    op <- sub("in_", "out_", tp, fixed = TRUE); tmp <- paste0(op, ".part")
    terra::writeRaster(o, tmp, filetype = "GTiff", overwrite = TRUE, datatype = "INT2S", NAflag = -1L,
                       gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
    file.rename(tmp, op)                                      # atomic publish
    op
  }

  if (length(todo) && identical(engine, "threaded")) {
    for (tp in todo) one_tile(tp, tile_fn, bands, nlyr)
  } else if (length(todo)) {
    mirai::daemons(n, .compute = "coverpred")
    on.exit(mirai::daemons(0, .compute = "coverpred"), add = TRUE)
    mirai::everywhere({
      for (p in PKGS) suppressMessages(library(p, character.only = TRUE))
      assign("TILE_FN", tile_fn, envir = globalenv()); assign("BANDS", bands, envir = globalenv())
      assign("NLYR", nlyr, envir = globalenv()); assign("ONE_TILE", one_tile, envir = globalenv())
    }, tile_fn = tile_fn, bands = bands, nlyr = nlyr, one_tile = one_tile, PKGS = packages,
    .compute = "coverpred")
    res <- mirai::mirai_map(todo, function(tp) ONE_TILE(tp, TILE_FN, BANDS, NLYR), .compute = "coverpred")
    invisible(res[])                                          # blocks until all tiles done
  }

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  vrt <- terra::vrt(outtiles)
  if (is.null(writer)) {
    terra::writeRaster(vrt, out_path, overwrite = TRUE, datatype = "INT2S", NAflag = -1L,
                       gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES"))
  } else {
    writer(vrt)
  }
  unlink(tdir, recursive = TRUE)
  out_path
}


#' Scene dissimilarity-index raster (parallel over mirai daemons)
#'
#' DI is stored as INT2S x 1000: values above 32.767 would overflow to NA and read
#' as "no data" rather than "beyond the AOA" (review 2026-09-22; current maxima are
#' 14-21), so the DI is clamped at 32.767 before the write.
#'
#' @param cube_path satellite cube; @param bands predictor bands
#' @param di_obj output of `cover_di()`; @param out_path output; @param aoi crop/mask
#' @return `out_path`
predict_di_raster <- function(cube_path, bands, di_obj, out_path, aoi = NULL, aggregate = 1L) {
  cube <- terra::rast(cube_path)[[bands]]
  if (aggregate > 1L) cube <- terra::aggregate(cube, fact = aggregate, fun = "mean", na.rm = FALSE)
  di_of <- di_obj$di_of
  tile_fn <- function(v) matrix(pmin(round(di_of(v) * 1000), 32767), ncol = 1L)
  raster_predict_parallel(cube, aoi, out_path, tile_fn, nlyr = 1L, engine = "daemon")
  ok <- system2("gdal_edit.py", c("-scale", "0.001", "-offset", "0", shQuote(out_path)),
                stdout = FALSE, stderr = FALSE)
  if (!identical(ok, 0L)) warning("gdal_edit.py did not tag the DI scale on ", out_path, call. = FALSE)
  out_path
}


#' Neltuma cover area from the scene surface: naive, within-AOA, and PPI-corrected
#'
#' Area = sum(cover x pixel) over the study area (naive) and over the AOA only
#' (label-supported). The within-AOA cover is PPI-corrected by the model's bias on
#' the labelled drone cells, but STRATIFIED BY PREDICTED COVER rather than a single
#' global rectifier (revised 2026-09-21 [HUGH]).
#'
#' Why stratified: the ensemble's bias is strongly regime-dependent - it under-
#' predicts DENSE cover (e.g. WV2 approx -10 pp in the 0.1-0.25 band, driven by the
#' one dense site struizendam_4) and slightly over-predicts SPARSE cover. A single
#' global delta smears the dense-site bias across a mostly-sparse scene, and the
#' site bootstrap of that scalar then drove the WV2 lower bound to a spurious ZERO
#' (P(area=0) ~ 5.5%) - impossible given ~160M label-supported cells. Instead we bin
#' scene and OOF cells by PREDICTED cover into `strata_edges` bands and correct each
#' band by its own OOF bias (fallback to the global delta for bands with
#' < `min_stratum_n` OOF cells), mirroring the DI-stratified conformal. This is a
#' LOCAL rectifier: the dense-band correction only touches the few dense scene cells.
#'
#' The interval is a SITE CLUSTER BOOTSTRAP percentile CI: with leave-site-out folds
#' the SITE is the unit of spatial independence, so we resample sites and recompute
#' the per-stratum biases (cell-weighted, so the CI is centred on the point estimate,
#' unlike the earlier unweighted per-site-mean bootstrap). Positive-bounded and
#' asymmetric by construction. Earlier rejected shapes: naive theta(1-theta)/N_pixel
#' (treats autocorrelated pixels as independent -> collapses) and a symmetric SE
#' clamped at 0. The per-pixel conformal bounds remain the MAP uncertainty; this is
#' the AGGREGATE.
#'
#' @param cover_path scene cover raster ([0,1] via scale tag)
#' @param di_path scene DI raster
#' @param threshold AOA DI threshold
#' @param oof ensemble OOF `list(row_ids, response, truth)` on the drone cells
#' @param train_df cover table (its `site` column, aligned to `oof$row_ids`)
#' @param aoi study-area vector path
#' @param px_ha ignored (kept for the call signature); the pixel area is read from the raster
#' @param sensor label
#' @param alpha CI level
#' @param strata_edges interior predicted-cover breakpoints for the local rectifier
#' @param min_stratum_n min OOF cells for a stratum's own bias (else global delta)
#' @return one-row data.frame of areas
cover_scene_area <- function(cover_path, di_path, threshold, oof, train_df, aoi, px_ha,
                             sensor = NA_character_, alpha = 0.05,
                             strata_edges = c(0.02, 0.05, 0.10, 0.25),
                             min_stratum_n = 200L) {
  v <- terra::vect(aoi)
  cover <- terra::mask(terra::rast(cover_path), v)
  di <- terra::mask(terra::rast(di_path), v)
  px_ha <- prod(terra::res(cover)) / 1e4          # from the raster (aggregated in the fast profile)
  cv <- terra::values(cover)[, 1]; dv <- terra::values(di)[, 1]
  inside <- !is.na(dv) & dv <= threshold                   # AOA (DI-supported) cells
  ok <- inside & !is.na(cv)
  covA <- cv[ok]                                           # predicted cover on AOA cells
  scene_cells  <- sum(!is.na(cv))
  inside_cells <- sum(inside)
  naive_ha  <- sum(cv, na.rm = TRUE) * px_ha
  aoa_ha    <- sum(covA) * px_ha
  tot_in    <- inside_cells * px_ha
  theta_in  <- aoa_ha / tot_in                             # within-AOA mean cover

  # ---- STRATIFIED (regime-aware) bias rectifier (see header) ----
  brks <- c(-Inf, strata_edges, Inf); nb <- length(brks) - 1L
  d <- oof$response - oof$truth
  delta <- mean(d)                                         # global fallback + reported bias
  ob  <- factor(cut(oof$response, brks, labels = FALSE), levels = seq_len(nb))
  scb <- factor(cut(covA,        brks, labels = FALSE), levels = seq_len(nb))
  cnt <- tapply(covA, scb, length); sm <- tapply(covA, scb, sum)   # scene cells / cover per stratum
  cnt[is.na(cnt)] <- 0; sm[is.na(sm)] <- 0
  bias_k <- tapply(d, ob, mean); n_k <- tapply(d, ob, length)
  bk <- as.numeric(bias_k)
  bk[is.na(bk) | is.na(n_k) | n_k < min_stratum_n] <- delta        # thin-stratum fallback
  # corrected area = sum_k max(sum_cover_k - bias_k * n_k, 0) * px_ha
  ppi_ha <- sum(pmax(as.numeric(sm) - bk * as.numeric(cnt), 0)) * px_ha

  # ---- CI: site cluster bootstrap of the per-stratum biases ----
  site <- factor(train_df$site[oof$row_ids]); n_sites <- nlevels(site)
  ssum <- tapply(d, list(site, ob), sum); scnt <- tapply(d, list(site, ob), length)
  ssum[is.na(ssum)] <- 0; scnt[is.na(scnt)] <- 0
  smv <- as.numeric(sm); cntv <- as.numeric(cnt)
  set.seed(1L); B <- 4000L
  area_b <- vapply(seq_len(B), function(b) {
    pick <- sample.int(n_sites, n_sites, replace = TRUE)
    num <- colSums(ssum[pick, , drop = FALSE]); den <- colSums(scnt[pick, , drop = FALSE])
    delta_b <- sum(num) / sum(den)                     # global fallback from THIS draw
    bkb <- ifelse(den >= min_stratum_n, num / den, delta_b)
    sum(pmax(smv - bkb * cntv, 0)) * px_ha
  }, numeric(1))
  ci <- stats::quantile(area_b, c(alpha / 2, 1 - alpha / 2), names = FALSE)

  # Sensitivity flagged by the 2026-09-22 statistical review: the dense-stratum
  # biases are measured with the one dense site held out, so the correction's
  # sign rests on that site. Report how much of the bootstrap falls below the
  # UNCORRECTED within-AOA sum, and the threshold's provenance.
  rule <- attr(threshold, "rule"); if (is.null(rule)) rule <- NA_character_
  data.frame(sensor = sensor, scene_ha = scene_cells * px_ha, aoa_ha = tot_in,
             aoa_frac = inside_cells / scene_cells, naive_ha = naive_ha,
             cover_aoa_ha = aoa_ha, ppi_ha = ppi_ha,
             ppi_lo_ha = ci[1], ppi_hi_ha = ci[2],
             p_below_aoa_sum = mean(area_b < aoa_ha),
             bias_pp = 100 * delta, site_bias_sd_pp = 100 * stats::sd(tapply(d, site, mean)),
             threshold = as.numeric(threshold), threshold_rule = rule,
             n_sites = n_sites, n_overlap = length(d), stringsAsFactors = FALSE)
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
#' full cover table, predict the satellite cube tile by tile (threaded single-copy
#' engine; ranger through the compiled traversal), average the per-learner
#' responses (equal weight, clamped to [0, 1]) and write the cover raster.
#'
#' @param train_df cover table from `cover_training_table()`
#' @param cube_path satellite predictor cube
#' @param bands predictor band names
#' @param learner_ids regr twin ids
#' @param out_path output cover raster path
#' @param epsg CRS code for the task
#' @param aoi study-area vector path (predict the study area only)
#' @return `out_path`
predict_cover_scene <- function(train_df, cube_path, bands, learner_ids, out_path,
                                epsg = 32734, aoi = NULL, aggregate = 1L) {
  data.table::setDTthreads(1L)
  models <- fit_cover_models(train_df, bands, learner_ids)
  nthr <- as.integer(Sys.getenv("NELTUMA_PREDICT_CORES", "8"))
  set_predict_threads(models, nthr)
  preds <- lapply(models, fast_predictor, nthreads = nthr)
  cube <- terra::rast(cube_path)
  if (!all(bands %in% names(cube))) {
    stop("Cube lacks band(s): ", paste(setdiff(bands, names(cube)), collapse = ", "),
         call. = FALSE)
  }
  tile_fn <- function(v) {
    dat <- as.data.frame(v); acc <- numeric(nrow(v))
    for (f in preds) acc <- acc + pmin(pmax(f(dat), 0), 1)
    matrix(round(acc / length(preds) * PROB_SCALE), ncol = 1L)
  }
  cube <- cube[[bands]]
  # fast profile: predict on an aggregated cube, as the hard-class arm does
  if (aggregate > 1L) cube <- terra::aggregate(cube, fact = aggregate, fun = "mean", na.rm = FALSE)
  raster_predict_parallel(cube, aoi, out_path, tile_fn, nlyr = 1L, engine = "threaded")
  tag_prob_scale(out_path)
  out_path
}


#' Set the per-model predict thread count where the learner exposes one
#'
#' ranger is predicted through the compiled traversal (threads passed directly);
#' lightgbm (`num_threads`) and xgboost (`nthread`) use their own pools; svm and
#' glmnet are single-threaded (9% / 5% of prediction CPU, measured).
#' @param models list of trained mlr3 learners (modified in place)
#' @param nthr threads
set_predict_threads <- function(models, nthr) {
  for (m in models) {
    ids <- m$param_set$ids()
    if ("num_threads" %in% ids) m$param_set$set_values(num_threads = nthr)
    if ("nthread" %in% ids) m$param_set$set_values(nthread = nthr)
  }
  invisible(models)
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
