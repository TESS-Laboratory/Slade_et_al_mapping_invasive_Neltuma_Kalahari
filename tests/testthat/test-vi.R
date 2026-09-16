testthat::test_that("vi_formulas reproduce the shipped index names and ranges", {
  g <- c(300, 500); r <- c(400, 800); n <- c(900, 1200)
  v <- vi_formulas(g, r, n)
  testthat::expect_named(v, c("msavi", "msavi2", "mtvi", "ndvi", "savi"))
  testthat::expect_equal(v$ndvi, (n - r) / (n + r))
  testthat::expect_equal(v$savi, 1.5 * (n - r) / (n + r + 0.5))
  # finding 4.10: 'msavi' carries 2*red, 'msavi2' is the Qi et al. form
  testthat::expect_equal(v$msavi2, (2 * n + 1 - sqrt((2 * n + 1)^2 - 8 * (n - r))) / 2)
  testthat::expect_false(isTRUE(all.equal(v$msavi, v$msavi2)))
})
