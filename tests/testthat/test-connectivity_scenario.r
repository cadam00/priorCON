test_that("connectivity_scenario works", {
  library(priorCON)

  # Read connectivity files from folder and combine them
  combined_edge_list <- preprocess_graphs(system.file("external",
                                          package="priorCON"),
                                          header = FALSE, sep =";")

  # Set seed for reproducibility
  set.seed(42, "Mersenne-Twister", sample.kind="Rejection")

  # Detect graph communities using the s-core algorithm
  pre_graphs <- get_metrics(combined_edge_list, which_community = "s_core")

  cost_raster <- get_cost_raster()
  features_rasters <- get_features_raster()
  # Solve an ordinary prioritizr prioritization problem
  connectivity_solution <- connectivity_scenario(
                             cost_raster=cost_raster,
                             features_rasters=features_rasters,
                             budget_perc=0.1,
                             pre_graphs=pre_graphs
                           )

  expect_equal(terra::values(connectivity_solution$solution)[1:10],
               c(NA,NA,NA,NA,NA,NA,NA,NA,1,1)
  )

  # Without features
  combined_edge_list2 <- combined_edge_list
  combined_edge_list2[,1] <- "f2"
  combined_edge_list <- rbind(combined_edge_list, combined_edge_list2)
  # Set seed for reproducibility
  set.seed(42, "Mersenne-Twister", sample.kind="Rejection")

  # Detect graph communities using the s-core algorithm
  pre_graphs <- get_metrics(combined_edge_list, which_community = "s_core")

  # Check locked_in argument, e.g. select the 100th cell.
  locked_in  <- cost_raster * 0
  locked_out <- cost_raster * 0
  locked_in[100]  <- locked_out[101]  <- TRUE

  connectivity_solution <- connectivity_scenario(cost_raster=cost_raster,
                                                 features_rasters=NULL,
                                                 budget_perc=0.1,
                                                 pre_graphs=pre_graphs,
                                                 locked_in=locked_in)

  expect_equal(terra::values(connectivity_solution$solution)[100], 1)

  # Check locked_out argument, e.g. exclude the 101st one.

  connectivity_solution <- connectivity_scenario(cost_raster=cost_raster,
                                                 features_rasters=NULL,
                                                 budget_perc=0.1,
                                                 pre_graphs=pre_graphs,
                                                 locked_out=locked_out)

  expect_equal(terra::values(connectivity_solution$solution)[101], 0)

  # # Solve an ordinary prioritizr prioritization problem
  # connectivity_solution <- connectivity_scenario(cost_raster=cost_raster,
  #                                                features_rasters=NULL,
  #                                                budget_perc=0.5,
  #                                                pre_graphs=pre_graphs)
  #
  # expect_equal(terra::values(connectivity_solution$solution)[1:10],
  #              c(NA,NA,NA,NA,NA,NA,NA,NA,1,1)
  # )

  # Error case
  connectivity_solution <- try(connectivity_scenario(
                                 cost_raster=cost_raster,
                                 features_rasters=features_rasters,
                                 budget_perc=10,
                                 pre_graphs=pre_graphs
                               ),
                           silent = TRUE)

  expect_true(is(connectivity_solution, "try-error"), TRUE)
  expect_equal(connectivity_solution[[1]],
               paste0("Error in connectivity_scenario(",
               "cost_raster = cost_raster, features_rasters = features_rasters",
               ",  : \n  budget_perc must be a numeric value between 0 and 1\n")
  )
})

test_that("normf works", {
  # Create two dummy SpatRaster objects
  # The first has values 1:25
  r1 <- terra::rast(nrow=5, ncol=5)
  r1 <- terra::init(r1, fun="cell")

  # The second has values 100:2500
  r2 <- terra::rast(nrow=5, ncol=5)
  r2 <- terra::init(r2, fun="cell") * 100

  # Stack them into one SpatRaster
  r <- c(r1, r2)
  names(r) <- c("cell", "cell")

  # Normalize them
  # Now both have the same values and in [0-1] scale
  norm_r <- priorCON:::.normf(r)
  expect_equal(terra::values(norm_r),
               matrix((rep(1:25,2) - 1) / (25 - 1),
               ncol=2,
               dimnames=list(NULL, c("cell", "cell"))))

  # Case that max(r) == min(r)
  # It must return the input raster for the first layer
  r[[1]] <- r[[1]] / r[[1]]
  norm_r <- priorCON:::.normf(r)
  expect_equal(terra::values(norm_r),
               matrix(c(rep(1,25), ((1:25) - 1) / (25 - 1)),
               ncol=2,
               dimnames=list(NULL, c("cell", "cell"))))
})
