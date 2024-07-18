test_that("basic_scenario works", {
  library(priorCOM)
  
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
  
  # Error cases
  basic_solution <- try(basic_scenario(cost_raster=cost_raster,
                                       features_rasters=features_rasters,
                                       budget_perc=10),
                        silent = TRUE)
  
  expect_equal(class(basic_solution) == "try-error", TRUE)
  
  basic_solution <- try(basic_scenario(cost_raster=cost_raster,
                                       features_rasters=NULL,
                                       budget_perc=0.1),
                        silent = TRUE)
  
  expect_equal(class(basic_solution) == "try-error", TRUE)
  
})