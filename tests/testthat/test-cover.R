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

test_that("calibrate_neltuma_prob errors when the Neltuma column is absent", {
  sv <- list(row_ids = 1:3, prob = cbind(`0` = c(.2, .3, .4), `6` = c(.8, .7, .6)),
             truth = factor(c("0", "6", "0"), levels = c("0", "6")))
  expect_error(calibrate_neltuma_prob(sv, neltuma_code = 5L), "absent")
})
