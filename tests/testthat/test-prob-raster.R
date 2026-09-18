testthat::test_that("probability rasters round-trip through the INT16 scaling", {
  testthat::skip_if_not_installed("terra")
  r <- terra::rast(nrows = 4, ncols = 4, vals = runif(16))
  p <- file.path(tempdir(), "prob.tif")
  terra::writeRaster(terra::round(r * PROB_SCALE), p, overwrite = TRUE, datatype = "INT2S", NAflag = -1L)
  tag_prob_scale(p)
  back <- read_prob(p)
  testthat::expect_lt(max(abs(terra::values(back) - terra::values(r))), 1 / PROB_SCALE)
})

testthat::test_that("tag_prob_scale embeds a GDAL scale so read_prob returns [0,1] with no manual divide", {
  testthat::skip_if_not_installed("terra")
  testthat::skip_if(Sys.which("gdal_edit.py") == "")
  r <- terra::rast(nrows = 4, ncols = 4, vals = round(runif(16), 4))
  p <- file.path(tempdir(), "probtag.tif")
  terra::writeRaster(terra::round(r * PROB_SCALE), p, overwrite = TRUE, datatype = "INT2S", NAflag = -1L)
  tag_prob_scale(p)
  gi <- gdalinfo_scale <- system2("gdalinfo", p, stdout = TRUE)
  testthat::expect_true(any(grepl("Scale:0.0001", gi)))
  back <- read_prob(p)                       # no divide inside
  testthat::expect_lt(max(abs(terra::values(back) - terra::values(r))), 1 / PROB_SCALE)
  testthat::expect_lte(max(terra::values(back)), 1)
})
