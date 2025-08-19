test_that("graph_connectivity_rasters works", {
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

  # Planning Units SpatRaster object
  pu_raster <- get_cost_raster()

  # Get graph connectivity rasters
  f1_s_core <- graph_connectivity_rasters(pu_raster, pre_graphs)

  expect_equal(values(unname(f1_s_core)),
               values(unname(connectivity_solution$original_connectivity_rast)))

  # Read connectivity files from folder and combine them
  combined_edge_list <- preprocess_graphs(system.file("external",
                                                      package="priorCON"),
                                          header = FALSE, sep =";")

  # Set seed for reproducibility
  set.seed(42, "Mersenne-Twister", sample.kind="Rejection")

  combined_edge_list2 <- combined_edge_list
  combined_edge_list2[,1] <- "f2"
  combined_edge_list <- rbind(combined_edge_list, combined_edge_list2)

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

  # Planning Units SpatRaster object
  pu_raster <- get_cost_raster()

  # Get graph connectivity rasters
  f1_s_core <- graph_connectivity_rasters(pu_raster, pre_graphs)

  expect_equal(values(unname(f1_s_core)),
               values(unname(connectivity_solution$original_connectivity_rast)))


})
