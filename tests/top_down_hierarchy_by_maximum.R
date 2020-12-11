
#' Select the SBI hierarchy so that the codes only contain certain amount of companies
#'
#' This function can be used to to only get the tree (no. of companies) under our thershold that we set
#' so the codes represent enough of something for example number of companies (maybe later for number of customers or
#' revenue)
#'
#' @param tbl_SBI_count Graph representing the SBI relations in NL
#' @param threshold The maximum value name_attribute should have to be included for further top town
#' @return A graph that fit the requirement of no. of companies in the network
#' @keywords SBI
#' @export
#' @examples
#' top_down_hierarchy_by_maximum(tbl_SBI_count = tbl_SBI_count, threshold=10000)
top_down_hierarchy_by_maximum <- function(tbl_SBI_count = tbl_SBI_count, threshold=10000){

  # run this function first because it is not exported from the graydon package
  get_incoming_vertice_names <- function(graph, name_vertex, order = 1){

    # create a small subnetwork of the vertx_hierarchy and its child vertices
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

  # create graph with SBI info
  names(tbl_SBI_count)[which(names(tbl_SBI_count) == "code_SBI")] <- "code"
  names(tbl_SBI_count)[which(names(tbl_SBI_count) == "code_SBI_parent")] <- "code_parent"
  names(tbl_SBI_count)[which(names(tbl_SBI_count) == "hierarchy_layer")] <- "layer_no"
  names(tbl_SBI_count)[which(names(tbl_SBI_count) == "qty_companies")] <- "attribute_no"

  ## create vertices
  vertices <- with(tbl_SBI_count, unique(c(code, code_parent)))
  tbl_vertices <- data.frame(code = vertices, stringsAsFactors = FALSE) %>%
    dplyr::left_join(tbl_SBI_count, by = "code")

  ## create edges
  tbl_edges <- tbl_SBI_count %>% dplyr::select(code, code_parent, tidyselect::everything())

  ## create graph
  graph_hierarchy <- igraph::graph_from_data_frame(d = tbl_edges,
                                                   vertices = tbl_vertices,
                                                   directed = TRUE)

  ## add root/layer information
  graph_hierarchy <- graydon.package::vertices_add_distance_to_root(graph_hierarchy)

  ## add the key attribute related to qty_companies in the graph
  graph_hierarchy <- graydon.package::propagate_hierarchy_value(graph = graph_hierarchy,
                                                                name_attribute = "attribute_no",
                                                                name_propagate = "qty_attribute_cum",
                                                                distance = Inf,
                                                                direction = "in",
                                                                FUN = sum,
                                                                na.rm = TRUE)

  # refine the graph with threshold
  max_distance <- max(igraph::V(graph_hierarchy)$layer_no, na.rm = TRUE)
  distance <- 1
  idx_delete <- c()
  idx_split <- c("0")

  ## to spilt/keep the nodes which are above the threshold
  while(length(idx_split) > 0 & distance < max_distance){
    ### create attributes for split/keep nodes
    is_distance <- igraph::V(graph_hierarchy)$dist_to_root == distance
    is_above_threshold <- igraph::V(graph_hierarchy)$qty_attribute_cum > threshold

    is_below_threshold <- igraph::V(graph_hierarchy)$qty_attribute_cum <= threshold

    idx_split <- igraph::V(graph_hierarchy)[is_distance & is_above_threshold]$name
    idx_keep <- igraph::V(graph_hierarchy)[is_distance & is_below_threshold]$name

    graph_hierarchy<- igraph::delete.vertices(graph = graph_hierarchy,
                                              v = igraph::V(graph_hierarchy)[idx_split])

    ### get max_distance layers of incoming vertices
    layer <- get_incoming_vertice_names(graph_hierarchy, idx_keep, order = max_distance)

    ### remove all edges with nodes lower than the selected level
    edges_below_degree <- unlist(igraph::incident_edges(graph_hierarchy,
                                                        v = igraph::V(graph_hierarchy)[layer],
                                                        mode = "out"))
    graph_hierarchy <- igraph::delete.edges(graph_hierarchy, edges = edges_below_degree)

    ### reconnect vertices
    new_edges <- as.vector(rbind(layer, rep(idx_keep, length(layer))))  # directly connect the name with each below degree node
    graph_hierarchy<- igraph::add.edges(graph_hierarchy, edges = new_edges)


    distance = distance + 1
  }

  ## make a label to show in the plot because label is a shown attribute in igraph
  V(graph_hierarchy)$label <- paste0(V(graph_hierarchy)$name,
                                     '\n',
                                     V(graph_hierarchy)$qty_attribute_cum,
                                     ' - ',
                                     V(graph_hierarchy)$attribute_no)

  return(graph_hierarchy)
}
