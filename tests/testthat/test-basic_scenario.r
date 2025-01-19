test_that("basic_scenario works", {
  library(priorCON)

  # Set seed for reproducibility
  set.seed(42, "Mersenne-Twister", sample.kind="Rejection")

  cost_raster <- get_cost_raster()
  features_rasters <- get_features_raster()
  # Solve an ordinary prioritizr prioritization problem
  basic_solution <- basic_scenario(cost_raster=cost_raster,
                                   features_rasters=features_rasters,
                                   budget_perc=0.1)

  expect_equal(terra::values(basic_solution$solution)[1:10],
               c(NA,NA,NA,NA,NA,NA,NA,NA,0,1)
  )

  # Check locked_in argument, e.g. select the 100th cell.
  locked_in  <- cost_raster * 0
  locked_out <- cost_raster * 0
  terra::values(locked_in)[100]  <- terra::values(locked_out)[101] <- 1

  basic_solution <- basic_scenario(cost_raster=cost_raster,
                                   features_rasters=features_rasters,
                                   budget_perc=0.1,
                                   locked_in=locked_in)

  expect_equal(terra::values(basic_solution$solution)[100], 1)

  # Check locked_out argument, e.g. exclude the 101st one.

  basic_solution <- basic_scenario(cost_raster=cost_raster,
                                   features_rasters=features_rasters,
                                   budget_perc=0.1,
                                   locked_out=locked_out)

  expect_equal(terra::values(basic_solution$solution)[101], 0)

  # Error cases
  basic_solution <- try(basic_scenario(cost_raster=cost_raster,
                                       features_rasters=features_rasters,
                                       budget_perc=10),
                        silent = TRUE)

  expect_true(is(basic_solution, "try-error"))
  expect_equal(basic_solution[[1]],
               paste0("Error in basic_scenario(cost_raster = cost_raster, ",
               "features_rasters = features_rasters,  : \n  budget_perc must ",
               "be a numeric value between 0 and 1\n")
               )

  basic_solution <- try(basic_scenario(cost_raster=cost_raster,
                                       features_rasters=NULL,
                                       budget_perc=0.1),
                        silent = TRUE)

  expect_true(is(basic_solution, "try-error"))
  expect_equal(basic_solution[[1]],
               paste0("Error in basic_scenario(cost_raster = cost_raster, ",
                      "features_rasters = NULL,  : \n  No feature is given. ",
                      "Please provide at least 1 feature.\n")
  )

})
