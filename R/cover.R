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
