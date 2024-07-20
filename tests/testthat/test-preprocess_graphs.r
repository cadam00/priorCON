test_that("preprocess_graphs works", {
  combined_edge_list <- priorCON::preprocess_graphs(system.file("external",
                                          package="priorCON"),
                                          header = FALSE, sep =";")
  expect_equal(
  head(combined_edge_list),
  data.frame(feature="f1",
             from.X=22.623093921258,
             from.Y=40.3034242177313,
             to.X=c(22.623093921258,  22.6230939212579, 22.6230939212579,
                    22.6230939212579, 22.6230939212579, 22.6526634187221),
             to.Y=c(40.3034242177313, 40.3914420015487, 40.4134114057637,
                    40.4353668202755, 40.4573082585537, 40.3034242177315),
             weight=c(0, 0, 0, 0.005, 0, 0)
             )
  )
})