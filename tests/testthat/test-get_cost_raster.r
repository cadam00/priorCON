test_that("get_cost_raster works", {
  cost_raster <- priorCOM::get_cost_raster()
  expect_equal(terra::values(cost_raster)[1:10],
            c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,1,1)
            )
  expect_equal(terra::ext(cost_raster)[1:4],
    c(xmin = 22.6230939212579,
    xmax = 22.9187888958989,
    ymin = 40.3034242177313,
    ymax = 40.6323382317027
    )
  )
  expect_equal(dim(cost_raster), c(16, 11, 1))
})