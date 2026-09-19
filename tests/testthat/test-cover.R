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
