#' Add the distance to each vertex to the graph's root
#'
#' @param graph The graph to do the searhing in
#' @param vertex_attribute the name you want the vertex distance attribute to have
#' @return Graph
#' @keywords SBI NACE SIC
#' @example
#' vertex_names <- get_incoming_vertice_names(graph, threshold = 500)
vertices_add_distance_to_root <- function(graph, vertex_attribute = "dist_to_root"){

  vertx_root <- get_root_vertex(graph) # Determine the root vertice

  # Distances between root and nodes in the hierarchy, so values are calculated from leafs to root order
  igraph::vertex_attr(graph, vertex_attribute) <-
    igraph::distances(graph, v = igraph::V(graph), to = vertx_root)

  return(graph)
}

#' Get the names of the vertices that are children of the given vertex
#'
#' @param graph The graph to do the searhing in
#' @param name_vertex The name of the vertex to get the children from
#' @param order The number of connections to search through to get the children
#' @return List of vertex names that are incoming
#' @keywords SBI NACE SIC
#' @example
#' vertex_names <- get_incoming_vertice_names(graph, threshold = 500)
get_incoming_vertice_names <- function(graph, name_vertex, order = 1){

  # Create a small subnetwork of the vertx_hierarchy and it's child vertices
  vertx_incoming <- igraph::ego(graph = graph,
                                order = order,
                                nodes = igraph::V(graph)[name_vertex],
                                mode = "in")[[1]]
  vertx_incoming <- igraph::difference(vertx_incoming,
                                       igraph::V(graph)[name_vertex])
  idx_incoming <- names(vertx_incoming)
  rm(vertx_incoming)

  return(idx_incoming)
}

#' Roll up economic activity hierarchy so that the codes hold a minimum value
#'
#' This function can be used to aggregate a NACE or SBI code economic activity tree,
#' so the codes represent enough of something for example number of companies, number of customers or
#' revenue.
#'
#' @param graph_tree Graph representing the economic activity hierarchy
#' @param name_attribute The name of the attribute you want to base the roll-up on
#' @param name_propagated The name of the new attribute that should contain the cumulative value of name_attribute
#' @param threshold The minimum value name_attribute should have to be exluded from further roll-up
#' @return A data frame containing the original economic activity code, the new activity code and the quantity/value that the new code would contain if aggregated
#' @keywords SBI NACE SIC
#' @export
#' @example
#' graph_SBI_rolled <- roll_up_hierarchy_by_minimum(graph_tree = graph_SBI,
#'                                                  name_attribute = "qty_companies",
#'                                                  name_propagated = "qty_companies_cum",
#'                                                  threshold = 5000)
roll_up_hierarchy_by_minimum <- function(graph_tree, name_attribute, name_propagated, threshold){

  # Create new variable, name_propagated, filling with 0's
  igraph::vertex_attr(graph_tree, name_propagated) <- igraph::vertex_attr(graph_tree, name_attribute)

  vertx_root <- get_root_vertex(graph_tree)
  graph_tree <- vertices_add_distance_to_root(graph_tree)

  # Iterate through each node in the network from leaf to root order
  for(name_inward in igraph::V(graph_tree)$name[sort(igraph::V(graph_tree)$dist_to_root,
                                                     decreasing = TRUE,
                                                     index.return = TRUE)$ix]) {
    # Get 2 layers of incoming vertices
    idx_incoming <- get_incoming_vertice_names(graph_tree, name_inward, order = 2)

    # Remove vertices with 0 cumulative values
    is_cum_0 <- igraph::vertex_attr(graph_tree, name_propagated) == 0
    idx_cum_0 <- igraph::vertex_attr(graph_tree, "name")[is_cum_0]
    vertx_cum_0 <- igraph::intersection(igraph::V(graph_tree)[idx_incoming],
                                        igraph::V(graph_tree)[idx_cum_0[!is.na(idx_cum_0)]])
    graph_tree <- igraph::delete.vertices(graph_tree, v = igraph::V(graph_tree)[vertx_cum_0])
    rm(vertx_cum_0, idx_cum_0, is_cum_0)

    # Remove all first degree edges
    edges_1st_degree <- unlist(igraph::incident_edges(graph_tree,
                                                      v = igraph::V(graph_tree)[name_inward],
                                                      mode = "in"))
    graph_tree <- igraph::delete.edges(graph_tree, edges = edges_1st_degree)

    # Remove all second degree edges where the first degree nodes have cumulative value lower than threshold
    is_cum_below <- igraph::vertex_attr(graph_tree, name_propagated) < threshold

    # Gather all vertices without outgoing connections and lower than threshold values
    idx_no_connections <- igraph::V(graph_tree)[igraph::degree(graph_tree, mode = 'out') == 0]$name
    is_below_threshold <- igraph::vertex_attr(graph_tree, name_propagated) < threshold
    idx_below_threshold <- igraph::vertex_attr(graph_tree, "name")[is_below_threshold]
    idx_connect <- idx_no_connections[idx_no_connections %in% idx_below_threshold]
    idx_connect <- idx_connect[idx_connect != vertx_root$name]
    rm(idx_no_connections, is_below_threshold, idx_below_threshold)

    # Reconnect vertices with values below threshold
    new_edges <- as.vector(rbind(idx_connect, rep(name_inward, length(idx_connect))))
    graph_tree <- igraph::add.edges(graph_tree, edges = new_edges)

    # Cumulate values below threshold
    values_cumulative <- igraph::vertex_attr(graph_tree,
                                             name_propagated,
                                             index = igraph::V(graph_tree)[idx_connect])
    value_vertx <- igraph::vertex_attr(graph_tree,
                                       name_propagated,
                                       index = igraph::V(graph_tree)[name_inward])
    value_cumulative <- sum(values_cumulative + value_vertx)
    rm(values_cumulative, value_vertx)
    if (value_cumulative >= threshold & !is.na(value_cumulative)) {
      igraph::vertex_attr(graph_tree,
                          name_propagated,
                          index = igraph::V(graph_tree)[name_inward]) <- value_cumulative
    }
  }

  # Connect all root vertices with itself (to indicate there is no change)
  idx_no_connections <- igraph::V(graph_tree)[igraph::degree(graph_tree, mode = 'out') == 0]$name
  new_edges <- as.vector(rbind(idx_no_connections, idx_no_connections))
  graph_tree <- igraph::add.edges(graph_tree, edges = new_edges)

  # Assign values to edges
  edges <- igraph::E(graph_tree)[inc(igraph::V(graph_tree))]
  igraph::edge_attr(graph_tree, name_attribute, index = edges) <-
    igraph::vertex_attr(graph_tree, name_attribute, index = edges)
  igraph::edge_attr(graph_tree, name_propagated, index = edges) <-
    igraph::vertex_attr(graph_tree, name_propagated, index = edges)

  # Marking root vertices
  vertx_roots <- get_root_vertex_names(graph_tree)
  graph_tree <- mark_companies_logical(graph_tree,
                                       "is_root",
                                       "name",
                                       vertx_roots)
  return(graph_tree)
}

#' A function for enriching each of the codes from it's chosen level's code value
#'
#' @param tbl_hierarchy A data frame that should contain the entire NACE or SBI hierarchy.
#' @param level_no The hierarchy level that you'd want to add the code of
#' @param col_code The name of the column containing the NACE, SBI or SIC code.
#' @param col_code_parent The name of the column  that contains a NACE, SBI or SIC code that refers to the direct parent code.
#' @param col_layer_no The name of the column that contains an integer indicating the hierarchy's level, 1 being the top level
#' @return a data frame containing the original economic activity code and the new activity code
#' @keywords SBI NACE SIC
#' @export
#' @example
#' hierarchy_code_level(tbl_hierarchy, level_no = 2)
hierarchy_code_level <- function(tbl_hierarchy,
                                 level_no,
                                 col_code = "code",
                                 col_code_parent = "code_parent",
                                 col_layer_no = "layer_no"){

  # Rename columns for processing within the function
  names(tbl_hierarchy)[which(names(tbl_hierarchy) == col_code)] <- "code"
  names(tbl_hierarchy)[which(names(tbl_hierarchy) == col_code_parent)] <- "code_parent"
  names(tbl_hierarchy)[which(names(tbl_hierarchy) == col_layer_no)] <- "layer_no"

  # Codes that are pushed up the tree
  tbl_level_stored <- data.frame(code = as.character(),
                                 code_parent = as.character())

  # Iterator for the levels in the hierarchy, from top to specified level
  levels <- sort(unique(tbl_hierarchy$layer_no))

  for (i in levels[-level_no]) {

    # Get the current level
    tbl_level <- tbl_hierarchy %>%
      dplyr::filter(layer_no == i)  %>%
      dplyr::select(code, code_parent)

    # Push parent_code to the next level
    tbl_level_next <- tbl_level %>%
      dplyr::left_join(tbl_level_stored, by = c("code_parent" = "code")) %>%
      dplyr::mutate(code_parent = ifelse(!is.na(code_parent.y), code_parent.y, code_parent)) %>%
      dplyr::select(code, code_parent)

    # Store result
    tbl_level_stored <- rbind(tbl_level_stored, tbl_level_next)
  }

  # Rename column for selected level code
  names(tbl_hierarchy)[which(names(tbl_hierarchy) == "code_parent")] <- "code_new"

  # Add code back to original data frame
  tbl_hierarchy %<>%
    dplyr::left_join(tbl_level_stored, by = "code") %>%
    select(code, code_new)

  names(tbl_hierarchy)[which(names(tbl_hierarchy) == "code_new")] <- paste0(col_code, "_new")

  return(tbl_hierarchy)
}

#' cleans up all nace codes from the hierarchy that contain a NA value and are non-connective
#'
#' @param graph_tree The graph containing the hierarchical tree
#' @param name_attribute name of the attribute you want to evaluate to be non-zero
#' @return A graph containing only the codes with other values than NA and are non-connecting
#' @keywords SBI NACE SIC
#' @export
#' @example
#' graph_SBI_clean <- graph_remove_empty_non_connecting(graph_SBI, name_attribute = "qty_companies")
graph_remove_empty_non_connecting <- function(graph_tree, name_attribute) {

  vertx_root <- get_root_vertex(graph_tree) # Determine the root vertice

  # Distances between root and nodes in the hierarchy
  igraph::V(graph_tree)$dist_to_root <- igraph::distances(graph_tree,
                                                          v = igraph::V(graph_tree),
                                                          to = vertx_root)

  # Determine the calculation order based on ascending distance from root
  id_inward_vertices <- igraph::V(graph_tree)$name[sort(igraph::V(graph_tree)$dist_to_root,
                                                        decreasing = TRUE,
                                                        index.return = TRUE)$ix]

  # Iterate through each company in the network
  for(id_inward in id_inward_vertices) {

    # The company vertice
    vertx <- igraph::V(graph_tree)[id_inward]

    # Create a small subnetwork of the vertx_hierarchy and it's child vertices
    ego <- igraph::ego(graph = graph_tree, order = 500, nodes = vertx, mode = "in")
    vertx_incoming <- igraph::V(graph_tree)[ego[[1]]]
    value <- sum(igraph::vertex_attr(graph_tree, name_attribute)[vertx_incoming], na.rm = TRUE)

    if(value == 0) {
      graph_tree <- igraph::delete.vertices(graph_tree, vertx)
    }
  }

  return(graph_tree)
}

#' Get the root node of a tree
#'
#' @param tree_graph The graph containing the hierarchical tree
#' @return The vertext that is the root of the graph
#' @keywords SBI NACE SIC
#' @export
#' @example
#' graph_SBI <- get_root_vertex(graph_SBI)
get_root_vertex <- function(tree_graph){

  # Find root node
  idx_root <- which(sapply(sapply(igraph::V(tree_graph),
                                  function(x) igraph::neighbors(tree_graph, x, mode="out")),
                           length) == 0)
  vertx_root <- igraph::V(tree_graph)[idx_root]
  rm(idx_root)

  return(vertx_root)
}

#' Get the root node of a tree
#'
#' @param tree_graph The graph containing the hierarchical tree
#' @return The vertext that is the root of the graph
#' @keywords SBI NACE SIC
#' @export
#' @example
#' vertx_roots <- get_root_vertex_names(tbl_SBI_count)
get_root_vertex_names <- function(tree_graph){

  # Find root nodes
  tree_graph <- igraph::simplify(tree_graph)

  idx_roots <- which(
    sapply(
      sapply(igraph::V(tree_graph),
             function(x) igraph::neighbors(tree_graph,x, mode="out")
      ),
      length)
    == 0)

  vertx_roots <- igraph::V(tree_graph)[idx_roots]$name

  return(vertx_roots)
}
