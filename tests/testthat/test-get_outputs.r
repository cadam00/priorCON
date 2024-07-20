test_that("get_outputs works", {
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
  # Solve a connectivity prioritizr prioritization problem
  connectivity_solution <- connectivity_scenario(cost_raster=cost_raster,
                                   features_rasters=features_rasters,
                                   budget_perc=0.1, pre_graphs=pre_graphs)

  connectivity_outputs <- get_outputs(solution = connectivity_solution,
                                      feature = "f1", pre_graphs = pre_graphs)

  df_test <- data.frame(feature="f1",
                      relative_held=0.16372088810969578909,
                      "connections(%)"=0.33398855393393295232)
  names(df_test)[3] <- "connections(%)"

  expect_equal(connectivity_outputs$connectivity_table,
               df_test
  )

  # Solve an ordinary connectivity prioritizr prioritization problem
  basic_solution <- basic_scenario(cost_raster=cost_raster,
                                   features_rasters=features_rasters,
                                   budget_perc=0.1)

  basic_outputs <- get_outputs(solution = basic_solution, feature = "f1",
                                      pre_graphs=pre_graphs)

  df_test <- data.frame(feature="f1",
                        relative_held=0.17856298331018261027,
                        "connections(%)"=0)
  names(df_test)[3] <- "connections(%)"

  expect_equal(basic_outputs$connectivity_table,
               df_test
  )

  # Test loose and patch
  connectivity_outputs <- get_outputs(solution = connectivity_solution,
                                      feature = "f1", pre_graphs = pre_graphs,
                                      loose = TRUE, patch = TRUE)

  df_test <- data.frame(feature="f1",
                      relative_held=0.16372088810969578909,
                      "connections(%)"=1.9065179953728668139)
  names(df_test)[3] <- "connections(%)"

  expect_equal(connectivity_outputs$connectivity_table,
               df_test
  )

  nonas <- terra::values(connectivity_outputs$solution)
  nonas <- nonas[!is.na(nonas)]
  expect_equal(nonas,
               c(1,1,2,2,3,4,4,4,4,4,5)
  )

  connectivity_solution <- connectivity_scenario(cost_raster=cost_raster,
                                   features_rasters=NULL,
                                   budget_perc=0.1, pre_graphs=pre_graphs)

  connectivity_outputs <- get_outputs(solution = connectivity_solution,
                                      feature = "f1", pre_graphs=pre_graphs)

  df_test <- data.frame(feature="f1",
                      relative_held=NA,
                      "connections(%)"=0.21222189364551985458,
                      row.names="f1")
  names(df_test)[3] <- "connections(%)"

  expect_equal(connectivity_outputs$connectivity_table,
               df_test
  )

  combined_edge_list[,1] <- "no_correspondance"
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
                             budget_perc=0.1, pre_graphs=pre_graphs
                           )

  connectivity_outputs <- get_outputs(solution   = connectivity_solution,
                                      feature    = "no_correspondance",
                                      pre_graphs = pre_graphs)

  expect_equal(nrow(connectivity_outputs$connectivity_table), 3)
})
