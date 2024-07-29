get_cost_raster <- function()
  terra::rast(system.file("rasters/cost_raster.tif", package="priorCON"))
get_features_raster <- function()
  terra::rast(system.file("rasters/features_raster.tif", package="priorCON"))

#normalize
.normf <- function(x){
  minmaxs     <- terra::minmax(x, compute=TRUE)
  max_value   <- minmaxs[2,]
  min_value   <- minmaxs[1,]
  for (i in seq_len(terra::nlyr(x))){
    if (max_value[i] == min_value[i]) next
    x[[i]] <- (x[[i]]-min_value[i])/(max_value[i]-min_value[i])
  }
  return(x)
}

get_metrics <- function(connect_mat, which_community="s_core"){

  # Future use of adjacency matrix
  # if (is.list(connect_mat) && !is.data.frame(connect_mat)){
  #   features_list <- vector("list", length(connect_mat))
  #   names_connect_mat <- names(connect_mat)
  #   for (hh in seq_len(length(connect_mat))){
  #     features_list[[hh]] <- as.data.frame.table(
  #       as.matrix(connect_mat[[hh]]))[,c(2,1,3)]
  #     features_list[[hh]] <-
  #       data.frame(from.X=gsub("[,].*", "", features_list[[hh]][,1]),
  #                  from.Y=gsub("[,].*", "", features_list[[hh]][,2]),
  #                  to.X=gsub(".*[,]", "", features_list[[hh]][,1]),
  #                  to.Y=gsub(".*[,]", "", features_list[[hh]][,2]),
  #                  weight = features_list[[hh]][,3]
  #     )
  #     features_list[[hh]] <- data.frame(habitat=names_connect_mat[hh],
  #                                      features_list[[hh]])
  #   }
  # } else {
  features_list <- split(connect_mat, connect_mat$feature)
  # }

  graph_list <- memberships <- merged_coords <- vector("list",
                                                       length(features_list))
  single_coordinates <- do.call(rbind, features_list)
  single_coordinates <- cbind(c(single_coordinates$from.X,
                                single_coordinates$to.X),
                              c(single_coordinates$from.Y,
                                single_coordinates$to.Y))
  single_coordinates <- single_coordinates[!duplicated(single_coordinates),]

  for (hh in seq_len(length(features_list))){

    from <- merge(
      data.frame(features_list[[hh]],
                 coordcol = paste0(features_list[[hh]][,"from.X"], " ",
                                   features_list[[hh]][,"from.Y"]),
                 fororder=seq_len(nrow(features_list[[hh]]))),
      data.frame(single_coordinates,
                 id = seq_len(nrow(single_coordinates)),
                 coordcol = paste0(single_coordinates[,1], " ",
                                   single_coordinates[,2])),
      by = "coordcol", all.x=TRUE)
    from <- from[order(from$fororder),]


    to   <- merge(
      data.frame(features_list[[hh]],
                 coordcol = paste0(features_list[[hh]][,"to.X"], " ",
                                   features_list[[hh]][,"to.Y"]),
                 fororder=seq_len(nrow(features_list[[hh]]))),
      data.frame(single_coordinates,
                 id = seq_len(nrow(single_coordinates)),
                 coordcol = paste0(single_coordinates[,1], " ",
                                   single_coordinates[,2])),
      by = "coordcol", all.x=TRUE)
    to <- to[order(to$fororder),]


    merged_coords[[hh]] <- data.frame(features_list[[hh]],
                                      from=from$id, to=to$id)[,c(1,7,8,2:6)]
    features_list[[hh]] <- data.frame(features_list[[hh]],
                                     from=from$id, to=to$id)[,c(1,7,8,6)]


    result <- .get_polygons(features_list[[hh]],
                            which_community=which_community)
    memberships[[hh]] <- result[[2]]
    graph_list[[hh]] <- result[[3]]

  }
  return(list(which_community = which_community, graph_list = graph_list,
              memberships = memberships, features_list = features_list,
              merged_coords = merged_coords))
}


.get_polygons <- function(edge_list_i,  which_community="s_core"){

  edge_list_i <- edge_list_i[2:4]
  colnames(edge_list_i) <- c("from","to","weight")

  directed_graph_wgt <- graph_from_data_frame(edge_list_i, directed = TRUE)
  net_result <- as.undirected(directed_graph_wgt, mode = "collapse",
                              edge.attr.comb = "sum")

  # Simplifying graph
  net_result<- simplify(
    net_result,
    remove.multiple = TRUE,
    remove.loops = TRUE,
    edge.attr.comb = igraph_opt("edge.attr.comb")
  )


  #s_core values
  if (which_community == "s_core"){
    solution2 <- s_core(net_result, W=NULL)
  } else if (which_community == "louvain"){

    clust_result_louvain <- cluster_louvain(net_result)
    cs_louvain           <- sizes(clust_result_louvain)
    louvain_memb  <- membership(clust_result_louvain)
    mem_value <- rep(NA, length(louvain_memb))
    for (i in (seq_len(length(louvain_memb)))){
      mem_value[i] <- cs_louvain[louvain_memb[i][[1]]][[1]]
    }
    solution2 <- mem_value


  } else if (which_community == "walktrap"){
    #https://link.springer.com/chapter/10.1007/11569596_31
    #https://iopscience.iop.org/article/10.1088/1742-5468/2008/10/P10008/pdf
    clust_result_walk <- cluster_walktrap(net_result, steps = 4)
    cs_walk <- sizes(clust_result_walk)

    walk_memb <- membership(clust_result_walk)

    mem_value <- rep(NA, length(walk_memb))
    for (i in (seq_len(length(walk_memb)))){
      mem_value[i] <- cs_walk[walk_memb[i][[1]]][[1]]
    }

    solution2  <- mem_value

  } else if (which_community == "eigen"){

    solution2 <- unname(eigen_centrality(net_result)[[1]])

  } else if (which_community == "betw"){

    solution2 <- unname(betweenness(net_result))

  } else if (which_community == "deg"){

    #degree (in-out, total for unweighted) strength is just the weighted degree
    solution2 <- unname(strength(net_result))

  } else {

    stop(paste0("Community name must be one of 's_core', 'louvain',",
                " 'walktrap', 'eigen', 'betw' or 'deg'"))

  }

  return(list(community_name=which_community,solution2,net_result))

}


.spatial_plot_function <- function(graph_result,pu_raster){

  points_object <- st_as_sf(as.data.frame(pu_raster, xy = TRUE),
                                coords = c("x", "y"),
                                crs = crs(pu_raster))
  PUID <- points_object$PUID
  matrix_test <- as_data_frame(graph_result, what="edges")
  if (is.numeric(PUID) || is.integer(PUID)){
    matrix_test[,1] <- as.numeric(matrix_test[,1])
    matrix_test[,2] <- as.numeric(matrix_test[,2])
  }
  edges_list <- vector(mode="list", length=nrow(matrix_test))
  values<- numeric(nrow(matrix_test))
  for (i in seq_len(nrow(matrix_test))){
    first_point   <- which(matrix_test[i,1] == PUID)
    second_point  <- which(matrix_test[i,2] == PUID)
    # if (length(first_point) == 0 | length(second_point) == 0) {
    #   edges_list[i] <- NA
    #   next
    # }
    edges_list[i] <- st_cast(st_combine(
      points_object[c(first_point,second_point),]), "LINESTRING")
    values[i] <- matrix_test[i,3]
  }


  sfc_lines            <- do.call(st_sfc, edges_list)
  st_crs(sfc_lines)    <- st_crs(points_object)
  ## We do these manipulations in order to add the values of the connectivity
  ## matrix in the links
  sfc_lines                <- st_as_sf(sfc_lines)
  sfc_lines$`edge weights` <- values
  # spatial_graph_result <- as_sfnetwork(
  #                           sfc_lines[!is.na(edges_list),],#sfc_lines
  #                           directed=FALSE)
  #
  #
  # result_edges <- st_as_sf(spatial_graph_result, "edges",
  #                              crs = st_crs(points_object))
  # colnames(result_edges)[4]<-"edge weights"

  return(sfc_lines)

}


## Function for counting connectivities
## Runs after scenarios
.solution_pol_and_mat <- function(solution,
                                  loose = FALSE){

  values_pu_raster <- values(solution$pu_raster)

  solution_polygons        <- aggregate(values(solution$solution),
                                        list(values_pu_raster),
                                        "sum")
  names(solution_polygons) <- c("ID", "solution")

  which_patch <- terra::patches(solution$solution, directions=8, zeroAsNA=TRUE,
                                allowGaps=TRUE)

  if (loose) which_patch <- terra::clamp(which_patch, upper = 1)

  solution_patches <- split(values(which_patch), values_pu_raster)
  solution_patches <- vapply(solution_patches,
                             function(x)x[!is.na(x)][1],
                             numeric(1))
  solution_patches <- data.frame("ID" = seq_len(length(solution_patches)),
                                 "solution" = solution_patches)
  solution_patches <- solution_patches[!is.na(solution_patches$solution),]

  ## I consider chosen values as the same cluster : 1 , the others are 0
  solution_polygons$cluster                                  <- 0
  solution_polygons$cluster[solution_polygons$solution >= 1] <- 1

  output               <- solution_polygons
  output_chosen        <- output[output$cluster>0,]
  output_chosen_vector <- output_chosen$ID
  bad_vector           <- output[output$cluster==0,]$ID

  con_names             <- names(solution$features_list)

  modul_hh_matrix           <- data.frame(matrix(
    nrow=length(con_names), ncol=1))
  colnames(modul_hh_matrix) <- "connections(%)"

  maps_collection_inputs    <- list()

  for (hh in seq_len(length(con_names))){
    graph_input <- solution$graph_list[[hh]]
    initial_sum_connections <- sum(E(graph_input)$weight)

    edge_list_i  <- solution$features_list[[hh]]
    el           <- edge_list_i[
      !(edge_list_i$from %in% bad_vector) & !(edge_list_i$to %in% bad_vector),
      2:4]
    colnames(el) <- c("from","to","weight")

    indices_1 <- match(el[, 1], solution_patches$ID)
    indices_2 <- match(el[, 2], solution_patches$ID)
    el[, 4]   <- solution_patches[indices_1, 2]
    el[, 5]   <- solution_patches[indices_2, 2]
    el        <- el[el$V4 == el$V5, 1:3]
    el        <- el[stats::complete.cases(el),]

    directed_graph_wgt       <- graph_from_data_frame(el, directed = TRUE)
    net_result               <- as.undirected(directed_graph_wgt,
                                              mode = "collapse",
                                              edge.attr.comb = "sum")
    net_result               <- simplify(net_result, remove.multiple = TRUE,
                                         remove.loops = TRUE,
                                         edge.attr.comb =
                                           igraph_opt("edge.attr.comb"))
    solution_sum_connections <- sum(E(net_result)$weight)

    modul_hh_matrix[hh,1] <- 100 * solution_sum_connections /
      initial_sum_connections
    maps_collection_inputs[[hh]] <- list(hh,net_result, which_patch)
    names(maps_collection_inputs)[hh] <- con_names[hh]
  }

  modul_hh_matrix               <- modul_hh_matrix

  colnames(modul_hh_matrix)     <- "connections(%)"

  rownames(modul_hh_matrix)     <- con_names

  solution_polygons_raster          <- solution$pu_raster * 1
  solution2                         <- values_pu_raster
  solution2[!is.na(solution2)]      <- solution_polygons$solution
  solution_polygons_raster$solution <- solution2

  solution2[!is.na(values_pu_raster)] <- solution_polygons$cluster
  solution_polygons_raster$cluster    <- solution2

  return(list(solution, solution_polygons_raster, modul_hh_matrix,
              maps_collection_inputs))

}

basic_scenario <- function(cost_raster, features_rasters, budget_perc){

  if ((budget_perc < 0) || (budget_perc > 1))
    stop("budget_perc must be a numeric value between 0 and 1")

  total_budget <- as.numeric(global(cost_raster, "sum", na.rm=TRUE))
  available_budget<-(budget_perc)*total_budget

  if (!is.null(features_rasters)){
    stacked_f <- .normf(features_rasters)
  } else {
    stop("No feature is given. Please provide at least 1 feature.")
  }

  p0 <-
    problem(cost_raster, stacked_f) |>
    add_max_utility_objective(available_budget) |>
    add_binary_decisions() |>
    add_relative_targets(0.17) |>
    add_default_solver(verbose=FALSE)

  solution <- NULL
  solution$solution <- suppressWarnings(solve(p0, force = TRUE))
  names(solution$solution) <- "basic scenario"
  table_results_s0 <- eval_target_coverage_summary(p0, solution$solution)

  if (!is.null(features_rasters)){
    solution$table_results <- table_results_s0
  }

  attr(solution, "scenario") <- "basic"

  return(solution)

}

connectivity_scenario <- function(cost_raster, features_rasters=NULL,
                                  budget_perc, pre_graphs){

  if ((budget_perc < 0) || (budget_perc > 1))
    stop("budget_perc must be a numeric value between 0 and 1")

  r <- cost_raster * 0
  single_coordinates <- do.call(rbind, pre_graphs$merged_coords)
  single_coordinates <- cbind(c(single_coordinates$from.X,
                                single_coordinates$to.X),
                              c(single_coordinates$from.Y,
                                single_coordinates$to.Y),
                              c(single_coordinates$from,
                                single_coordinates$to)
  )
  single_coordinates <- as.matrix(single_coordinates[
                                   !duplicated(single_coordinates),])
  pu_raster <- terra::rasterize(single_coordinates[,c(1,2)],
                                r,
                                single_coordinates[,3],
                                fun=min)
  names(pu_raster)   <- "PUID"

  polygons_subset <- pu_raster * 1
  names(polygons_subset) <- "PUID"

  rep_ncell_NA <- rep(NA, ncell(polygons_subset))

  for (i in seq_len(length(pre_graphs$features_list))){

    net_result <- pre_graphs$graph_list[[i]]

    V_net_result_name <- V(net_result)$name
    if (is.numeric(pu_raster$PUID[1][[1]])){
      V_net_result_name <- as.numeric(V(net_result)$name)
    }

    values_polygons_subsetPUID <- values(pu_raster$PUID)

    # Remove vertices that are absent in the PU raster
    net_result <- delete_vertices(net_result,
                                  V(net_result)$name[!V_net_result_name %in%
                                                    values_polygons_subsetPUID]
    )

    V_net_result_name <- V(net_result)$name
    if (is.numeric(pu_raster$PUID[1][[1]])){
      V_net_result_name <- as.numeric(V(net_result)$name)
    }

    condition_raster <- pu_raster$PUID %in% V_net_result_name
    condition_values <- values(condition_raster)
    if (i == 1){
      polygons_subset  <- ifel(condition_raster,
                               pu_raster$PUID, NA)
    } else {

      add(polygons_subset)  <- ifel(condition_raster,
                                    pu_raster$PUID, NA)
    }

    # Make clustering in the same order with PUID
    V_corrected           <- data.frame(V_net_result_name,
                                        seq_len(length(V_net_result_name)))
    rownames(V_corrected) <- V_net_result_name
    correct_order         <- V_corrected[as.character(
                               values_polygons_subsetPUID[condition_values]),2]

    solution2 <- rep_ncell_NA
    solution2[condition_values] <- pre_graphs$memberships[[i]][correct_order]

    values(polygons_subset[[i]]) <- solution2

    pre_graphs$graph_list[[i]] <- net_result

  }

  choose_cluster <- polygons_subset
  names(choose_cluster) <- paste0(pre_graphs$which_community, "_",
                                  seq_len(nlyr(choose_cluster)))

  if (!is.null(features_rasters)){
    stacked_f_and_cluster <- .normf(c(features_rasters, choose_cluster))
  } else {
    stacked_f_and_cluster <- .normf(choose_cluster)
  }


  #budget problem:
  total_budget     <- as.numeric(global(cost_raster, "sum", na.rm=TRUE))
  available_budget <- budget_perc*total_budget
  p_cluster <-
    problem(cost_raster, stacked_f_and_cluster) |>
    add_max_utility_objective(available_budget) |>
    add_binary_decisions() |>
    add_relative_targets(0.17) |>
    add_default_solver(verbose=FALSE)

  solution <- NULL
  solution$solution <- suppressWarnings(solve(p_cluster, force=TRUE))
  names(solution$solution) <- "connectivity scenario"
  if (!is.null(features_rasters)){
    table_results_core <-
      eval_target_coverage_summary(p_cluster, solution$solution)
    solution$table_results <- table_results_core[
                                        seq_len(nlyr(features_rasters)),]
  }

  solution$pu_raster <- pu_raster

  solution$features_list <- pre_graphs$features_list

  solution$graph_list <- pre_graphs$graph_list

  attr(solution, "scenario") <- "connectivity"

  return(solution)

}

.evaluate_connectivity <- function(solution){

  if (!is.null(solution[[1]]$table_results)){
    solution$table_results <- as.data.frame(solution[[1]]$table_results)
    matrix_shiny_com <- rbind(solution$table_results[,c(1,8)])
    matrix_shiny_com <- merge(
      matrix_shiny_com,
      data.frame(feature=rownames(solution[[3]]), solution[[3]]),
      by="feature",
      all=TRUE
    )
  } else {
    matrix_shiny_com <- data.frame(feature=rownames(solution[[3]]),
                                   relative_held= NA,
                                   solution[[3]])
  }

  if (nrow(matrix_shiny_com) > 1){
    matrix_shiny_com <- rbind(matrix_shiny_com,
                              data.frame(feature="mean",
                                         t(colMeans(
                                           matrix_shiny_com[,-1], na.rm=TRUE)))
    )
  }

  names(matrix_shiny_com)[3] <- "connections(%)"

  return(matrix_shiny_com)
}

get_outputs <- function(solution, feature, pre_graphs,
                        loose = FALSE, patch = FALSE){

  r <- solution$solution * 0
  single_coordinates <- do.call(rbind, pre_graphs$merged_coords)
  single_coordinates <- cbind(c(single_coordinates$from.X,
                                single_coordinates$to.X),
                              c(single_coordinates$from.Y,
                                single_coordinates$to.Y),
                              c(single_coordinates$from,
                                single_coordinates$to)
  )
  single_coordinates <- as.matrix(
    single_coordinates[!duplicated(single_coordinates),])
  pu_raster <- terra::rasterize(single_coordinates[,c(1,2)],
                                r,
                                single_coordinates[,3],
                                fun=min)
  names(pu_raster)   <- "PUID"

  polygons_subset <- pu_raster * 1
  names(polygons_subset) <- "PUID"

  for (i in seq_len(length(pre_graphs$features_list))){

    net_result <- pre_graphs$graph_list[[i]]

    V_net_result_name <- V(net_result)$name
    if (is.numeric(polygons_subset$PUID[1][[1]])){
      V_net_result_name <- as.numeric(V(net_result)$name)
    }

    values_polygons_subsetPUID <- values(polygons_subset$PUID)

    # Remove vertices that are absent in the PU raster
    net_result <- delete_vertices(net_result,
                                  V(net_result)$name[!V_net_result_name %in%
                                                  values_polygons_subsetPUID]
    )

    pre_graphs$graph_list[[i]] <- net_result

  }

  solution$features_list <- pre_graphs$features_list

  solution$graph_list <- pre_graphs$graph_list

  solution$pu_raster <- pu_raster


  RFSP_all <- .solution_pol_and_mat(solution=solution, loose = loose)

  RFSP      <- RFSP_all[[4]][[feature]]
  gimi_plot <- .spatial_plot_function(RFSP[[2]], solution$pu_raster)

  RFSP[[3]] <- terra::patches(RFSP[[3]], directions=8, zeroAsNA=TRUE,
                              allowGaps=TRUE)

  values_RFSP_3 <- values(RFSP[[3]])

  values_RFSP_3[is.na(values_RFSP_3)] <- ""
  values_RFSP_3[values_RFSP_3!=""] <-
    paste0("patch ", values_RFSP_3[values_RFSP_3!=""])
  values_RFSP_3[values_RFSP_3==""] <- NA

  values(RFSP[[3]]) <- values_RFSP_3

  output <- NULL

  if(patch){
    output$tmap <- tm_shape(RFSP[[3]]) +
      tm_raster(col = "patches",
                title = "")

    output$solution <- RFSP[[3]]

  } else {
    no_patches <- RFSP[[3]] > 0
    output$tmap <- tm_shape(no_patches) +
      tm_raster(col = "patches", palette=terrain.colors(2)[1],
                labels = "Protected", drop.levels = TRUE,
                title = "")

    output$solution <- no_patches

  }

  output$tmap <- output$tmap +
    #tm_shape(gimi_plot[,4]) +
    tm_shape(gimi_plot) +
    tm_lines(col = "edge weights", scale=2, palette="magma", n=10)

  output$connections <- gimi_plot

  output$connectivity_table <- .evaluate_connectivity(solution = RFSP_all)

  return(output)
}

preprocess_graphs <- function(path, ...){
  folders_paths <- list.dirs(path = path, full.names = TRUE, recursive = FALSE)
  big_combined_edge_list <- data.frame(feature=NULL,
                                       from.X=NULL, from.Y=NULL,
                                       to.X=NULL, to.Y=NULL,
                                       weight=NULL)
  for (i in seq_len(length(folders_paths))){
    txt_files <- list.files(folders_paths[i], pattern = "\\.txt$|\\.csv$",
                            full.names = TRUE)
    number_of_data <- length(txt_files)
    txt_files_df <- lapply(txt_files, function(x, number_of_data) {
      as.data.frame(read.csv(file = x, ...)[seq_len(number_of_data),])},
      number_of_data = number_of_data)
    combined_edge_list <- do.call("rbind", txt_files_df)
    names(combined_edge_list) <- c("to.X","to.Y","weight")
    combined_edge_list <- cbind(feature  = basename(folders_paths[i]),
                                "from.X" = rep(txt_files_df[[1]][,1],
                                               each=number_of_data),
                                "from.Y" = rep(txt_files_df[[1]][,2],
                                               each=number_of_data),
                                combined_edge_list
                                )
    big_combined_edge_list <- rbind(big_combined_edge_list, combined_edge_list)
  }
  return(big_combined_edge_list)
}
