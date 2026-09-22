suppressMessages({library(mlr3); library(mlr3learners)})
test_that("flattened ranger probability forest reproduces ranger exactly", {
  skip_if_not_installed("ranger"); skip_if_not_installed("Rcpp")
  set.seed(11)
  n <- 400L
  d <- data.frame(a = rnorm(n), b = runif(n), c = rnorm(n, 2), d = rexp(n))
  d$y <- factor(ifelse(d$a + d$b * 2 + rnorm(n, 0, 0.5) > 1, "1", ifelse(d$c > 2.3, "5", "6")))
  m <- ranger::ranger(y ~ ., d, probability = TRUE, num.trees = 60, min.node.size = 3, seed = 2)
  new <- data.frame(a = rnorm(500), b = runif(500), c = rnorm(500, 2), d = rexp(500))
  ref <- predict(m, new)$predictions
  ff <- flatten_ranger(m)
  got <- predict_flat_forest(ff, new, nthreads = 2L)
  expect_equal(colnames(got), colnames(ref))
  expect_lt(max(abs(got - ref)), 1e-12)
  # column reorder helper via fast_predictor on an mlr3 learner
  task <- mlr3::as_task_classif(d, target = "y")
  l <- mlr3::lrn("classif.ranger", predict_type = "prob", num.trees = 40, seed = 3)
  l$train(task)
  lv <- rev(task$class_names)
  fp <- fast_predictor(l, lvls = lv, nthreads = 1L)
  expect_equal(attr(fp, "engine"), "flat")
  expect_lt(max(abs(fp(new) - l$predict_newdata(new)$prob[, lv])), 1e-12)
})

test_that("flattened ranger regression forest reproduces ranger exactly", {
  skip_if_not_installed("ranger"); skip_if_not_installed("Rcpp")
  set.seed(12)
  n <- 500L
  d <- data.frame(a = rnorm(n), b = runif(n), c = rnorm(n))
  d$y <- pmin(pmax(0.1 * d$a + 0.3 * d$b + rnorm(n, 0, 0.05), 0), 1)
  m <- ranger::ranger(y ~ ., d, num.trees = 50, seed = 4)
  new <- data.frame(a = rnorm(300), b = runif(300), c = rnorm(300))
  ref <- predict(m, new)$predictions
  got <- predict_flat_forest(flatten_ranger(m), new, nthreads = 2L)
  expect_lt(max(abs(got - ref)), 1e-12)
  task <- mlr3::as_task_regr(d, target = "y")
  l <- mlr3::lrn("regr.ranger", num.trees = 30, seed = 5); l$train(task)
  fp <- fast_predictor(l, nthreads = 1L)
  expect_equal(attr(fp, "engine"), "flat")
  expect_lt(max(abs(fp(new) - l$predict_newdata(new)$response)), 1e-12)
})

test_that("fast_predictor falls back to mlr3 for non-ranger learners", {
  set.seed(13)
  d <- data.frame(a = rnorm(200), b = runif(200)); d$y <- factor(ifelse(d$a > 0, "1", "2"))
  task <- mlr3::as_task_classif(d, target = "y")
  l <- mlr3::lrn("classif.featureless", predict_type = "prob"); l$train(task)
  fp <- fast_predictor(l, lvls = c("2", "1"))
  expect_equal(attr(fp, "engine"), "mlr3")
  p <- fp(d[1:5, ])
  expect_equal(colnames(p), c("2", "1"))
  expect_equal(dim(p), c(5L, 2L))
})
