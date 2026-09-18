testthat::test_that("ppi corrects the naive area by the measured bias with a CI", {
  testthat::skip_if_not_installed("terra")
  # scene: 100x100, 20% predicted Neltuma (class 1), rest class 2
  r <- terra::rast(nrows = 100, ncols = 100)
  v <- rep(2L, 10000); v[1:2000] <- 1L; terra::values(r) <- v
  p <- file.path(tempdir(), "cls.tif"); terra::writeRaster(r, p, overwrite = TRUE, datatype = "INT1U")
  # overlap confusion: sat over-predicts Neltuma. 1000 pixels:
  #   sat=1,drone=1: 300 ; sat=1,drone=2: 200 (false +) ; sat=2,drone=1: 50 (miss) ; sat=2,drone=2: 450
  conf <- data.frame(wv2_class = c(1,1,2,2), drone_class = c(1,2,1,2), n_pixels = c(300,200,50,450))
  out <- ppi_neltuma_area(p, conf, neltuma_code = 1L, sensor = "wv2")
  # naive fraction 0.20; bias delta = (a-b)/n, a=200, b=50 -> +0.15; ppi fraction = 0.05
  testthat::expect_equal(round(out$naive_ha / (out$n_scene * prod(terra::res(r)) / 1e4), 3), 0.2)
  testthat::expect_equal(round(out$bias_pp, 1), 15.0)
  testthat::expect_lt(out$ppi_ha, out$naive_ha)         # corrected down
  testthat::expect_lt(out$ppi_lo_ha, out$ppi_ha)
  testthat::expect_gt(out$ppi_hi_ha, out$ppi_ha)
})
