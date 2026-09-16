testthat::test_that("tidy_class_accuracy computes recall and precision for one class", {
  fake <- list(prediction = function() list(truth = factor(c(1, 1, 1, 2, 2, 3)),
                                            response = factor(c(1, 1, 2, 2, 1, 3))))
  r <- tidy_class_accuracy(fake, "s", "t", "l", code = 1L)
  testthat::expect_equal(r$recall, 2 / 3)
  testthat::expect_equal(r$precision, 2 / 3)
  testthat::expect_equal(r$n_truth, 3L)
  r2 <- tidy_class_accuracy(fake, "s", "t", "l", code = 9L)
  testthat::expect_true(is.na(r2$recall))
})
