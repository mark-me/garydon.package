#' Function for creating a graph for all company hierarchies
#'
#' This function greates an igraph from a data frame for making networks of company hierarchies.
#' For this function to work the first and second column must contain the company id and the
#' companies parent id respectively
#' @param tbl_company_relations A data frame containing the company/company relations data.
#' @return A graph with all company hierarchies
#' @importFrom magrittr "%>%"
#' @keywords graph company hierarchy
#' @export
#' @examples
#' graph_company_hierarchies <- create_graph_company_hierarchies(tbl_company_relations)
create_graph_company_hierarchies <- function(tbl_company_relations) {
  require(dplyr)

  # Create edges
  tbl_edges <- tbl_company_relations[,c(1,2)]
  tbl_edges <- tbl_edges[!is.na(tbl_edges[,2]),]
  colname_vertices <- names(tbl_edges)[1]
  tbl_edges[,colname_vertices] <- as.character(tbl_edges[,colname_vertices])

  # Create vertices
  tbl_vertices <- data.frame(unique(c(tbl_edges[][[1]],tbl_edges[][[2]])))
  names(tbl_vertices) <- colname_vertices
  tbl_vertices[,colname_vertices] <- as.character(tbl_vertices[,colname_vertices])
  tbl_vertices <- tbl_vertices %>%
    dplyr::left_join(tbl_company_relations, by = colname_vertices)

  # Create a graph containing all companies
  graph_company_hierarchies <-
    igraph::graph_from_data_frame(d = tbl_edges,
                                  vertices = tbl_vertices,
                                  directed = TRUE)

  return(graph_company_hierarchies)
}

#' Function to find a hierarchy for a company
#'
#' This function searches the graph of a company's complete hierarchy from a graph containing
#' a multitude of company hierarchies.
#' @param graph_all_companies A graph containing all company/company relations data.
#' @param id_company The id of the company of which you want to retrieve the whole hierarchy
#' @return A graph with the company hierarchy of the specifief company
#' @importFrom magrittr "%>%"
#' @keywords graph company hierarchy
#' @export
#' @examples
#' graph_company_hierarchy <- find_company_hierarchy(graph_company_hierarchies, "931238099")
find_company_hierarchy <- function(graph_all_companies, id_company){

  graph_found <- igraph::make_ego_graph(graph = graph_all_companies,
                                        nodes = igraph::V(graph_all_companies)[id_company],
                                        order = 900,
                                        mode = "all")
  return(graph_found[[1]])
}

#' Get the root node's name of a tree
#'
#' @param tree_graph The graph containing the hierarchical tree
#' @return The vertex name that is the root of the graph
#' @keywords graph company hierarchy
#' @export
#' @example
#' graph_SBI <- get_root_vertex_name(tbl_SBI_count, col_id = "code_SBI", col_id_parent = "code_SBI_parent")
get_root_vertex_name <- function(tree_graph){

  # Find root node
  idx_root <- which(sapply(sapply(igraph::V(tree_graph),
                                  function(x) igraph::neighbors(tree_graph, x, mode="out")),
                           length) == 0)
  vertx_root <- igraph::V(tree_graph)[idx_root]$name
  rm(idx_root)

  return(vertx_root)
}

#' Decomposes a graph with all company hierarchies to a list where the items contain
#' a single company hierarchy
#'
#' @param graph_company_hierarchies The graph containing all the hierarchical company trees
#' @return A list of company hierarchy graphs
#' @keywords graph company hierarchy
#' @export
#' @example
#' list_graphs <- list_company_hierarchy_graphs(graph_company_hierarchies)
list_company_hierarchy_graphs <- function(graph_company_hierarchies){

  # Create list of company hierarchy graphs
  list_graphs <- igraph::decompose.graph(graph_company_hierarchies)

  list_graphs <- lapply(list_graphs, add_company_hierarchy_stats)

  # df_rolled <- do.call(rbind, lapply(list_graphs_sample,
  #                                    igraph::as_data_frame,
  #                                    what = "vertices"))

  return(list_graphs)
}

get_siblings <- function(graph, idx_vertex){

  # Getting the parent
  vertx_parent <- names(igraph::ego(graph = graph,
                                    nodes = igraph::V(graph)[idx_vertex],
                                    mode = "out")[[1]])
  vertx_parent <- vertx_parent[vertx_parent != idx_vertex]

  # Getting the children of those parent
  vertx_siblings <- names(igraph::ego(graph = graph,
                                      nodes = igraph::V(graph)[vertx_parent],
                                      mode = "in")[[1]])

  # Excluding the idx_vertex, so only it's siblings are returned
  vertx_siblings <- vertx_siblings[vertx_siblings != vertx_parent]

  return(vertx_siblings)
}

get_siblings_df <- function(graph, idx_vertices){

  tbl_siblings <- data.frame(id_company = as.character(),
                            id_sibling = as.character())

  for(idx_vertex in idx_vertices){

    id_sibling <- get_siblings(graph, idx_vertex)
    id_company <- rep(idx_vertex, length(id_sibling))
    tbl_siblings <- rbind(tbl_siblings, data.frame(id_company, id_sibling))
  }

  # Added stats
  tbl_siblings %>%
    dplyr::mutate(is_self = id_company == id_sibling) %>%
    dplyr::group_by(id_company) %>%
    dplyr::mutate(qty_siblings = n()) %>%
    dplyr::ungroup()

  return(tbl_siblings)
}

#' Add hierarchy stats to all the vertices of a company hierarchy graph
#'
#' @param graph A graph
#' @return A graph where all the nodes contain information about the company hierarchy graph
#' @keywords graph company hierarchy
#' @export
#' @example
#' graph <- add_company_hierarchy_stats(graph)
add_company_hierarchy_stats <- function(graph){

  # Determine company hierarchy is a hierarchical tree
  igraph::vertex_attr(graph, "is_tree") <- igraph::is_dag(graph)

  # Determine the number of companies in the company hierarchies
  igraph::vertex_attr(graph, "qty_hierarchy_total") <- igraph::vcount(graph)

  # Determine top company
  igraph::vertex_attr(graph, "id_company_top") <- get_root_vertex_name(graph)

  # Distances between root and nodes in the hierarchy
  igraph::vertex_attr(graph, "distance_to_top") <-
    igraph::distances(graph, v = igraph::V(graph), to = get_root_vertex(graph))

  # Number of child companies for each company
  igraph::vertex_attr(graph, "qty_child_companies") <-
    sapply(sapply(igraph::V(graph),
                  function(x) igraph::neighbors(graph, x, mode="in")),
           length)

  # Number of sibling companies for each company
  igraph::vertex_attr(graph, "qty_siblings") <- length(get_siblings(graph, igraph::V(graph)$name))

  return(graph)
}

#' Adds an aggregate value to the vertices of a company hierarchy
#'
#' @param graph A graph
#' @param name_attribute The name of the value attribute to be aggregated
#' @param name_aggregate The name of the attribute where the aggregated value is stored
#' @param FUN the function which is used to calculate aggregated
#' @param ... The parameters passed to the function specified in FUN
#' @return A graph where all the nodes contain aggregated value
#' @keywords graph company hierarchy
#' @export
#' @example
#' graph <- aggregate_company_hierarchy_value(graph, name_attribute = "qty_employees", name_aggregate = "qty_employees_cum", FUN = sum, na.rm = TRUE)
aggregate_company_hierarchy_value <- function(graph, name_attribute, name_aggregate, FUN, ...){

  # Create new variable, name_propagated, filling with 0's
  igraph::vertex_attr(graph, name_aggregate) <- igraph::vertex_attr(graph, name_attribute)
  vertx_root <- get_root_vertex(graph) # Determine the root vertice
  vertex_distances <- igraph::distances(graph, v = igraph::V(graph), to = vertx_root)[, 1]
  idx_by_distances <- names(sort(vertex_distances, decreasing = TRUE))

  for(idx_company in idx_by_distances) {

    # Get incoming vertices
    idx_incoming <- c(idx_company, get_incoming_vertice_names(graph, idx_company))
    values_aggregate <- igraph::vertex_attr(graph,
                                            name_aggregate,
                                            index = igraph::V(graph)[idx_incoming])
    value_aggregate <- FUN(values_aggregate, ...)

    igraph::vertex_attr(graph,
                        name_aggregate,
                        index = igraph::V(graph)[idx_company]) <- value_aggregate
  }

  return(graph)
}

# igraph::plot.igraph(list_graphs_sample[[2]])
# igraph::plot.igraph(list_graphs[[2]])
#
# tbl_companies <- readRDS("~/R scripts/ana1965_graydon/Input/input_r_market_nl.RDS")
#
# tbl_temp <- df_rolled %>%
#   mutate(name = as.integer(name)) %>%
#   select(name) %>%
#   left_join(tbl_companies, by = c("name" = "id_graydon")) %>%
#   select(id_company = name,
#          id_company_parent = id_mothercompany,
#          code_sbi,
#          size_company = type_company,
#          qty_employees = number_employees
#          )

# tbl_company_relations_sample <- df_rolled2
# devtools::use_data(tbl_company_relations_sample, overwrite = TRUE)
