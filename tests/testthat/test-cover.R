test_that("Platt calibration reduces miscalibration and recovers the base rate", {
  set.seed(1)
  n <- 5000L
  q <- stats::plogis(stats::rnorm(n, -1, 1.5))          # true P(Neltuma), rare-ish
  y <- stats::rbinom(n, 1L, q)
  p <- stats::plogis(1.8 * stats::qlogis(q) + 0.7)       # over-confident, biased score
  sv <- list(row_ids = seq_len(n),
             prob = cbind(`0` = 1 - p, `5` = p),
             truth = factor(ifelse(y == 1L, "5", "0"), levels = c("0", "5")))

  g <- calibrate_neltuma_prob(sv, neltuma_code = 5L, method = "platt")
  pc <- g(p)

  # the calibrated mean tracks the true base rate better than the raw score
  expect_lt(abs(mean(pc) - mean(y)), abs(mean(p) - mean(y)))
  # and it is better calibrated (lower ECE)
  expect_lt(calibration_quality(pc, y)$ece, calibration_quality(p, y)$ece)
  # g maps into [0, 1] and is monotone in its input
  x <- seq(0.01, 0.99, by = 0.01)
  expect_true(all(g(x) >= 0 & g(x) <= 1))
  expect_true(all(diff(g(x)) >= -1e-9))
})

test_that("isotonic calibration is also monotone and improves ECE", {
  set.seed(2)
  n <- 4000L
  q <- stats::plogis(stats::rnorm(n, -0.5, 1.2))
  y <- stats::rbinom(n, 1L, q)
  p <- pmin(pmax(q^0.6, 1e-6), 1 - 1e-6)                 # non-logistic distortion
  sv <- list(row_ids = seq_len(n),
             prob = cbind(`0` = 1 - p, `5` = p),
             truth = factor(ifelse(y == 1L, "5", "0"), levels = c("0", "5")))

  g <- calibrate_neltuma_prob(sv, neltuma_code = 5L, method = "isotonic")
  pc <- g(p)
  expect_lt(calibration_quality(pc, y)$ece, calibration_quality(p, y)$ece)
  x <- seq(0.01, 0.99, by = 0.01)
  expect_true(all(diff(g(x)) >= -1e-9))
})

test_that("cover_training_table averages the CALIBRATED prob (mean(g(p)), not g(mean(p)))", {
  skip_if_not_installed("terra"); skip_if_not_installed("sf"); skip_if_not_installed("exactextractr")
  # drone Neltuma prob: two 5cm-ish cells, 0.1 and 0.9, over extent [0,2] x [0,1]
  prob <- terra::rast(nrows = 1, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 1)
  terra::values(prob) <- c(0.1, 0.9); names(prob) <- "prob_1"
  pf <- tempfile(fileext = ".tif"); terra::writeRaster(prob, pf, overwrite = TRUE)
  # satellite cube: one band "blue" = 5 everywhere
  cube <- terra::rast(nrows = 1, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 1)
  terra::values(cube) <- c(5, 5); names(cube) <- "blue"
  cf <- tempfile(fileext = ".tif"); terra::writeRaster(cube, cf, overwrite = TRUE)
  # one satellite pixel polygon covering both drone cells
  poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(
    rbind(c(0,0), c(2,0), c(2,1), c(0,1), c(0,0))))), crs = terra::crs(prob))
  gf <- tempfile(fileext = ".fgb"); sf::st_write(poly, gf, quiet = TRUE)

  g <- function(p) p^2                                  # nonlinear, monotone
  out <- cover_training_table(cf, pf, gf, "site_x", "5", g, nel_band = 1L)

  # mean(g(p)) = (0.1^2 + 0.9^2)/2 = 0.41 ; g(mean(p)) = 0.25 would be wrong
  expect_equal(out$training$cover, 0.41, tolerance = 1e-6)
  expect_equal(out$training$blue, 5, tolerance = 1e-9)
  expect_equal(nrow(out$training), 1L)          # pixel kept, no purity filter
  expect_true(all(c("cover","site","tag","x","y","blue") %in% names(out$training)))
})

test_that("cover regression OOF and ensemble align by row id and clamp to [0,1]", {
  skip_if_not_installed("mlr3"); skip_if_not_installed("mlr3spatiotempcv")
  skip_if_not_installed("mlr3learners"); skip_if_not_installed("ranger")
  suppressMessages({library(mlr3); library(mlr3learners)})
  set.seed(3)
  n <- 60L
  x1 <- stats::rnorm(n); x2 <- stats::rnorm(n)
  cover <- pmin(pmax(stats::plogis(1.5 * x1) + stats::rnorm(n, 0, 0.05), 0), 1)
  df <- data.frame(cover = cover, site = "s", tag = "t",
                   x = stats::runif(n, 0, 100), y = stats::runif(n, 0, 100),
                   b1 = x1, b2 = x2)
  task <- make_cover_task(df, "s", "t", epsg = 32734)
  # two-fold design where every row is tested exactly once
  folds <- list(train_sets = list(31:60, 1:30), test_sets = list(1:30, 31:60))

  rr_rf <- run_cover_resample(task, cover_learner("ranger"), folds)
  rr_fl <- run_cover_resample(task, mlr3::lrn("regr.featureless"), folds)
  o1 <- cover_oof(rr_rf); o2 <- cover_oof(rr_fl)

  expect_identical(o1$row_ids, 1:60)                    # every row once, sorted
  expect_true(all(o1$response >= 0 & o1$response <= 1)) # clamped to cover range

  ens <- cover_ensemble_oof(list(o1, o2))
  expect_identical(ens$row_ids, 1:60)
  # equal-weight average, aligned by row id
  expect_equal(ens$response, (o1$response + o2$response) / 2, tolerance = 1e-9)
  # ranger should beat the featureless baseline on this learnable signal
  rmse <- function(o) sqrt(mean((o$response - o$truth)^2))
  expect_lt(rmse(o1), rmse(o2))
})

test_that("DI-stratified conformal widens with DI and holds ~1-alpha coverage per bin", {
  set.seed(4)
  n <- 4000L
  di_cal <- stats::runif(n)                       # dissimilarity index in [0,1]
  spread <- 0.02 + 0.30 * di_cal                  # heteroscedastic in DI
  resid  <- stats::rnorm(n, 0, spread)            # OOF residuals

  # fresh held-out set from the same process, to check coverage honestly
  m <- 4000L
  di_new <- stats::runif(m)
  yhat_new <- rep(0.5, m)
  truth_new <- yhat_new + stats::rnorm(m, 0, 0.02 + 0.30 * di_new)

  b <- di_conformal_bounds(resid, di_cal, di_new, yhat_new, alpha = 0.10, n_bins = 5L)
  q <- attr(b, "q")
  # half-widths increase with the DI bin
  expect_true(all(diff(q) > 0))
  # empirical coverage ~ 0.90 overall and within each bin
  cov <- di_coverage(truth_new, b$lower, b$upper, b$bin)
  expect_gt(cov$overall, 0.86)
  expect_true(all(cov$by_bin$coverage > 0.82))
})

test_that("leave_site_out_folds gives one fold per site, each row tested once", {
  df <- data.frame(site = rep(c("a","b","c"), c(4,3,5)), cover = runif(12))
  f <- leave_site_out_folds(df)
  expect_length(f$test_sets, 3L)
  expect_setequal(unlist(f$test_sets), 1:12)              # every row tested once
  expect_true(all(mapply(function(tr,te) length(intersect(tr,te))==0,
                         f$train_sets, f$test_sets)))     # disjoint train/test
  expect_identical(sort(f$test_sets[[1]]), 1:4)           # site 'a' rows
})

test_that("cover_di (FNN) gives low DI in-distribution, high DI out, threshold separates", {
  skip_if_not_installed("FNN")
  set.seed(5); n <- 400L; bands <- c("b1", "b2", "b3")
  tr <- data.frame(b1 = rnorm(n), b2 = rnorm(n), b3 = rnorm(n))
  folds <- list(train_sets = list((n/2 + 1):n, 1:(n/2)),
                test_sets  = list(1:(n/2), (n/2 + 1):n))
  di <- cover_di(tr, bands, folds)
  expect_length(di$di_cal, n)
  expect_true(is.finite(di$threshold) && di$threshold > 0)
  ind <- data.frame(b1 = rnorm(50), b2 = rnorm(50), b3 = rnorm(50))       # in-distribution
  oud <- data.frame(b1 = rnorm(50, 8), b2 = rnorm(50, 8), b3 = rnorm(50, 8)) # far away
  expect_lt(mean(di$di_of(ind) > di$threshold), 0.2)   # in-dist mostly inside AOA
  expect_gt(mean(di$di_of(oud) > di$threshold), 0.9)   # out-of-dist outside AOA
})

test_that("cover_aoa derives a CV threshold and separates in/out-of-distribution", {
  skip_if_not_installed("CAST")
  set.seed(1); n <- 200L; bands <- c("b1", "b2", "b3")
  tr <- data.frame(b1 = rnorm(n), b2 = rnorm(n), b3 = rnorm(n),
                   cover = runif(n), site = "s", tag = "t",
                   x = runif(n), y = runif(n))
  folds <- list(train_sets = list(1:100, 101:200), test_sets = list(101:200, 1:100))
  newd <- data.frame(b1 = c(rnorm(30), rnorm(30, 8)),
                     b2 = c(rnorm(30), rnorm(30, 8)),
                     b3 = c(rnorm(30), rnorm(30, 8)))
  out <- cover_aoa(tr, bands, folds, newd)
  expect_length(out$di_cal, n)                    # DI per training point
  expect_true(is.finite(out$threshold))
  expect_gt(mean(as.numeric(out$aoa)[1:30]),  0.8) # in-distribution mostly inside
  expect_lt(mean(as.numeric(out$aoa)[31:60]), 0.2) # out-of-distribution outside
})

test_that("beyond the AOA threshold no interval is issued; sparse bins clamp to [0,1]", {
  resid <- stats::rnorm(200, 0, 0.05)
  di_cal <- stats::runif(200, 0, 0.5)
  di_new <- c(0.1, 0.4, 5.0)                       # last is outside the AOA
  b <- di_conformal_bounds(resid, di_cal, di_new, yhat_new = c(.5, .5, .5),
                           alpha = 0.1, n_bins = 4L, aoa_threshold = 1.0)
  expect_false(b$inside_aoa[3])
  expect_true(is.na(b$lower[3]) && is.na(b$upper[3]))
  expect_true(all(b$lower[1:2] >= 0 & b$upper[1:2] <= 1))
})

test_that("cover_ensemble_oof rejects learners with mismatched row ids", {
  o1 <- list(row_ids = 1:5, response = runif(5), truth = runif(5))
  o2 <- list(row_ids = 2:6, response = runif(5), truth = runif(5))
  expect_error(cover_ensemble_oof(list(o1, o2)), "row ids")
})

test_that("calibrate_neltuma_prob errors when the Neltuma column is absent", {
  sv <- list(row_ids = 1:3, prob = cbind(`0` = c(.2, .3, .4), `6` = c(.8, .7, .6)),
             truth = factor(c("0", "6", "0"), levels = c("0", "6")))
  expect_error(calibrate_neltuma_prob(sv, neltuma_code = 5L), "absent")
})

test_that("cover_scene_area: stratified rectifier keeps a positive-bounded CI", {
  skip_if_not_installed("terra")
  # Scene: mostly sparse (~1% cover) with a small dense patch (~30%).
  cover <- terra::rast(nrows = 100, ncols = 100, xmin = 0, xmax = 100,
                       ymin = 0, ymax = 100, crs = "EPSG:32734")
  cv <- rep(0.01, 10000); cv[1:500] <- 0.30
  terra::values(cover) <- cv
  di <- terra::deepcopy(cover); terra::values(di) <- rep(0.1, 10000)  # all in-AOA (< thr)
  cp <- file.path(tempdir(), "cov.tif"); dp <- file.path(tempdir(), "di.tif")
  terra::writeRaster(cover, cp, overwrite = TRUE)
  terra::writeRaster(di, dp, overwrite = TRUE)
  ap <- file.path(tempdir(), "aoi.fgb")
  poly <- terra::as.polygons(terra::ext(cover)); terra::crs(poly) <- "EPSG:32734"
  terra::writeVector(poly, ap, filetype = "FlatGeobuf", overwrite = TRUE)
  # OOF: 3 sparse sites slightly over-predicted; 1 dense-only site under-predicted.
  # A GLOBAL rectifier would let the dense site's -15pp bias swing the scalar and
  # the site bootstrap could clamp the lower bound to 0; the STRATIFIED rectifier
  # confines that bias to the few dense scene cells, so the lower bound stays > 0.
  resp  <- c(rep(0.01, 300), rep(0.30, 100))
  truth <- c(rep(0.005, 300), rep(0.45, 100))
  site  <- c(rep(c("a", "b", "c"), length.out = 300), rep("d", 100))
  oof <- list(row_ids = seq_len(400), response = resp, truth = truth)
  tr  <- data.frame(site = site)
  out <- cover_scene_area(cp, dp, threshold = 1, oof = oof, train_df = tr, aoi = ap,
                          px_ha = 0.01, sensor = "t", min_stratum_n = 20L)
  expect_true(all(c("ppi_ha", "ppi_lo_ha", "ppi_hi_ha") %in% names(out)))
  expect_gt(out$ppi_lo_ha, 0)                       # the fix: no spurious zero
  expect_lt(out$ppi_lo_ha, out$ppi_ha)              # lower bound below the point
  expect_lte(out$ppi_ha, out$ppi_hi_ha)             # point at or below the upper
})
