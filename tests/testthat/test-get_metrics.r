test_that("get_metrics works", {
  library(priorCON)
  # Read connectivity files from folder and combine them
  combined_edge_list <- preprocess_graphs(system.file("external",
                                          package="priorCON"),
                                          header = FALSE, sep =";")

  # Set seed for reproducibility
  set.seed(42, "Mersenne-Twister", sample.kind="Rejection")

  # Detect graph communities using the s-core algorithm
  pre_graphs <- get_metrics(combined_edge_list, which_community = "s_core")

  expect_equal(
    head(pre_graphs$merged_coords[[1]]),
    data.frame(feature="f1",
               from=1,
               to=1:6,
               from.X=22.623093921258,
               from.Y=40.3034242177313,
               to.X=c(22.623093921258,  22.6230939212579, 22.6230939212579,
                      22.6230939212579, 22.6230939212579, 22.6526634187221),
               to.Y=c(40.3034242177313, 40.3914420015487, 40.4134114057637,
                      40.4353668202755, 40.4573082585537, 40.3034242177315),
               weight=c(0, 0, 0, 0.005, 0, 0)
    )
  )

  expect_equal(
    length(pre_graphs$memberships[[1]]),
    112
  )

  # Detect graph communities using the louvain algorithm
  pre_graphs <- get_metrics(combined_edge_list, which_community = "louvain")

  expect_equal(
    pre_graphs$which_community,
    "louvain"
  )

  expect_equal(
    length(pre_graphs$memberships[[1]]),
    112
  )

  ## Can not check memberships, as noted by Szabolcs Horvát
  ## For more, see:
  ## https://github.com/cadam00/priorCON/issues/1
  # expect_equal(
  #   head(pre_graphs$memberships[[1]]),
  #   c(31, 48, 48, 48, 48, 31)
  # )

  # Detect graph communities using the walktrap algorithm
  pre_graphs <- get_metrics(combined_edge_list, which_community = "walktrap")

  expect_equal(
    head(pre_graphs$memberships[[1]]),
    c(112, 112, 112, 112, 112, 112)
  )

  # Detect graph communities using the eigen algorithm
  pre_graphs <- get_metrics(combined_edge_list, which_community = "eigen")

  expect_equal(
    head(pre_graphs$memberships[[1]]),
    c(0.35538369,0.56264975,0.56492280,0.57572578,0.57265610,0.40234984)
  )

  # Detect graph communities using the betw algorithm
  pre_graphs <- get_metrics(combined_edge_list[combined_edge_list$weight > 0,],
                          which_community = "betw")

  expect_equal(
    head(pre_graphs$memberships[[1]]),
    c(244.9709888,48.9569390,10.4839819,8.1294855,32.1611709,62.7235935)
  )

  # Detect graph communities using the deg algorithm
  pre_graphs <- get_metrics(combined_edge_list, which_community = "deg")

  expect_equal(head(pre_graphs$memberships[[1]]),
    c(8.030, 16.260, 16.185, 16.155, 15.160, 8.880)
  )

  # Error case
  pre_graphs <- try(get_metrics(combined_edge_list,
                                which_community = "hi"),
                                silent = TRUE)

  expect_equal(class(pre_graphs) == "try-error", TRUE)

})
