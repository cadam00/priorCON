test_that("get_features_raster works", {
  features_raster <- priorCON::get_features_raster()
  expect_equal(terra::values(features_raster)[1:10],
            c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,1.00008297,1.00008738)
  )
  expect_equal(terra::ext(features_raster)[1:4],
               c(xmin = 22.6230939212579,
               xmax = 22.9187888958989,
               ymin = 40.3034242177313,
               ymax = 40.6323382317027
               )
  )
  expect_equal(dim(features_raster), c(16, 11, 1))
})