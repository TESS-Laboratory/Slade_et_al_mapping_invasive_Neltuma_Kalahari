testthat::test_that("probability rasters round-trip through the INT16 scaling", {
  testthat::skip_if_not_installed("terra")
  r <- terra::rast(nrows = 4, ncols = 4, vals = runif(16))
  p <- file.path(tempdir(), "prob.tif")
  terra::writeRaster(terra::round(r * PROB_SCALE), p, overwrite = TRUE, datatype = "INT2S", NAflag = -1L)
  back <- read_prob(p)
  testthat::expect_lt(max(abs(terra::values(back) - terra::values(r))), 1 / PROB_SCALE)
})
