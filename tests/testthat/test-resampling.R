testthat::test_that("build_cv_design returns partitioning, disjoint, repeat-distinct kNNDM folds", {
  testthat::skip_if_not_installed("CAST")
  set.seed(1)
  n <- 120
  d <- data.frame(Type = factor(sample(1:3, n, TRUE)), b1 = rnorm(n),
                  x = runif(n, 500000, 500400), y = runif(n, 7000000, 7000300))
  task <- mlr3spatiotempcv::as_task_classif_st(d, target = "Type", id = "t", coordinate_names = c("x", "y"),
                                               crs = "EPSG:32734", coords_as_features = FALSE)
  dom <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(c(xmin = 500000, ymin = 7000000, xmax = 500400, ymax = 7000300),
                                                        crs = sf::st_crs(32734))))
  f <- file.path(tempdir(), "dom.fgb"); sf::st_write(dom, f, delete_dsn = file.exists(f), quiet = TRUE)
  eval <- list(seed = 1L, final = list(folds_unit = 5, folds_aoi = 3, repeats = 3, clustering = "kmeans", samplesize = 300L))
  tune <- list(seed = 1L, tuning = list(folds = 3))
  cv <- build_cv_design(task, f, "unit", eval, tune)
  testthat::expect_equal(length(cv$outer$test_sets), 5L * 3L)
  testthat::expect_equal(length(cv$W_outer), 3L)
  testthat::expect_true(all(is.finite(cv$W_outer)))
  for (i in seq_along(cv$outer$test_sets)) {
    testthat::expect_length(intersect(cv$outer$train_sets[[i]], cv$outer$test_sets[[i]]), 0L)
  }
  # each repeat partitions the task
  testthat::expect_setequal(unlist(cv$outer$test_sets[1:5]), task$row_ids)
  # repeats are not copies of each other (k-means, separately seeded)
  sig <- vapply(1:3, function(r) paste(sort(vapply(cv$outer$test_sets[((r - 1) * 5 + 1):(r * 5)], function(s) sum(s), 1)), collapse = "-"), "")
  testthat::expect_gt(length(unique(sig)), 1L)
  r <- as_custom_resampling(task, cv$inner)
  testthat::expect_equal(r$iters, 3L)
})
