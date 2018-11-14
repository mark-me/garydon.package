library(magrittr)
library(dplyr)
library(igraph)
library(graydon.package)

# Create SBI graph ----
graph_sub_tree <- create_SBI_tree(c("C"))

igraph::V(graph_sub_tree)$label <- paste0(igraph::V(graph_sub_tree)$name, "\n",
                                          igraph::V(graph_sub_tree)$qty_companies, "\n",
                                          igraph::V(graph_sub_tree)$qty_companies_cum)
graph_tree <- graph_sub_tree
name_attribute <- "qty_companies"
name_propagated <- "qty_companies_cum"
threshold <- 5000
plot_graydon_graph(graph_tree, main = "Before")

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

#plot_graydon_graph(graph_tree, main = paste("Removed 0 cumulatives", name_inward))

    # Remove all first degree edges
    edges_1st_degree <- unlist(igraph::incident_edges(graph_tree,
                                                      v = igraph::V(graph_tree)[name_inward],
                                                      mode = "in"))
    graph_tree <- igraph::delete.edges(graph_tree, edges = edges_1st_degree)
#plot_graydon_graph(graph_tree, main = paste("Removed 1st degree edges", name_inward))

#if(name_inward == "14") break
    # Remove all edges from nodes have cumulative value lower than threshold
    is_cum_below <- igraph::vertex_attr(graph_tree, name_propagated) < threshold
    idx_cum_below <- igraph::vertex_attr(graph_tree, "name")[is_cum_below]
    idx_cum_below <- idx_cum_below[idx_cum_below %in% idx_incoming]
    edges_below <- unlist(igraph::incident_edges(graph_tree,
                                                 v = igraph::V(graph_tree)[idx_cum_below],
                                                 mode = "in"))
    graph_tree <- igraph::delete.edges(graph_tree, edges = edges_below)
#plot_graydon_graph(graph_tree, main = paste("Removed below degree edges", name_inward))

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
#plot_graydon_graph(graph_tree, main = paste("Reconnected edges cum below threshold", name_inward))

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
#plot_graydon_graph(graph_tree, main = paste("Recalculated cumulatives", name_inward))

}

  # Connect all root vertices with itself (to indicate there is no change)
  idx_no_connections <- igraph::V(graph_tree)[igraph::degree(graph_tree, mode = 'out') == 0]$name
  new_edges <- as.vector(rbind(idx_no_connections, idx_no_connections))
  graph_tree <- igraph::add.edges(graph_tree, edges = new_edges)
plot_graydon_graph(graph_tree, main = "After", vertex.size = 3, edge.arrow.size = .2)
#
#   # Assign values to edges
#   edges <- igraph::E(graph_tree)[inc(igraph::V(graph_tree))]
#   igraph::edge_attr(graph_tree, name_attribute, index = edges) <-
#     igraph::vertex_attr(graph_tree, name_attribute, index = edges)
#   igraph::edge_attr(graph_tree, name_propagated, index = edges) <-
#     igraph::vertex_attr(graph_tree, name_propagated, index = edges)
#
  # Marking root vertices
  vertx_roots <- get_root_vertex_names(graph_tree)
  graph_tree <- mark_companies_logical(graph_tree,
                                       "is_root",
                                       "name",
                                       vertx_roots)

igraph::V(graph_tree)$color <- ifelse(igraph::V(graph_tree)$is_root, 1, 2)
plot_graydon_graph(graph_tree, main = "After", vertex.size = 3, edge.arrow.size = .2)
