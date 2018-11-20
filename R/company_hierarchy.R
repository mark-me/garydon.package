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

  # Create vertices
  tbl_vertices <- data.frame(unique(c(tbl_edges[][[1]],tbl_edges[][[2]])),
                             stringsAsFactors = FALSE)
  names(tbl_vertices) <- colname_vertices
  tbl_vertices <- tbl_vertices %>%
    dplyr::left_join(tbl_company_relations, by = colname_vertices)

  # Create a graph containing all companies
  graph_hierarchies <-
    igraph::graph_from_data_frame(d = tbl_edges,
                                  vertices = tbl_vertices,
                                  directed = TRUE)
  return(graph_hierarchies)
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

  vertx <- igraph::V(graph_all_companies)[id_company]

  graph_found <- igraph::make_ego_graph(graph = graph_all_companies,
                                        nodes = vertx,
                                        order = 900,
                                        mode = "all")

  graph_found <- mark_companies_logical(graph_found[[1]],
                                        "is_searched_company",
                                        "id_company",
                                        id_company)
  return(graph_found)
}

#' Function to find the hierarchies of a list of companies
#'
#' This function searches the graphs of a vector of companies for their complete
#' hierarchies from a graph containing containing a multitude of company hierarchies.
#'
#' The graphs will be unique, and contain a vertex attribute is_searched_company
#' which specifies whether it the vertex represents a company which was selected
#' @param graph_all_companies A graph containing all company/company relations data.
#' @param id_companies A vector of company id's of which you want to retrieve the whole hierarchy
#' @return A list of graphs containing the grawith the company hierarchy of the specifief company
#' @importFrom magrittr "%>%"
#' @keywords graph company hierarchy
#' @export
#' @examples
#' graph_company_hierarchy <- find_company_hierarchy(graph_all_companies, "931238099")
select_graph_hierarchies <- function(graph_all_companies, id_companies){

  list_selected <- list() # Will contain the list of selected graphs
  graph_keys <- vector(mode = "character", length = length(id_companies))
  i <- 1

  while(i <= length(id_companies)){

    id_company <- id_companies[i]
    # Find the graph associated with the company
    current_graph <- find_company_hierarchy(graph_all_companies, id_company)
    id_graph <- get_root_vertex_name(current_graph) # Get the root vertex (as id) of the graph
    # Only add when the graph isn't present yet
    if(is.na(match(id_graph, graph_keys))) {

      # Add the root identifier to the graph
      graph_keys[i] <- id_graph
      # Mark all companies that are being searched for in the retrieved graph
      current_graph <- mark_companies_logical(current_graph,
                                              "is_searched_company",
                                              "id_company",
                                              id_companies)
      list_selected[[id_company]] <- current_graph # Add to selected list
    }
    i <- i + 1
  }
  return(list_selected)
}

#' Get the root node's name of a tree
#'
#' @param tree_graph The graph containing the hierarchical tree
#' @return The vertex name that is the root of the graph
#' @keywords graph company hierarchy
#' @export
#' @examples
#' graph_SBI <- get_root_vertex_name(tree_graph)
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
#' @examples
#' list_graphs <- list_company_hierarchy_graphs(graph_company_hierarchies)
list_company_hierarchy_graphs <- function(graph_all_companies){

  # Create list of company hierarchy graphs
  list_graphs <- igraph::decompose.graph(graph_all_companies)

  list_graphs <- lapply(list_graphs, add_company_hierarchy_stats)

  return(list_graphs)
}

#' Converts a list of company hierarchy graphs to a data frame
#'
#' @param list_graphs The graph containing all the hierarchical company trees
#' @return A data frame with company hierarchy data
#' @keywords graph company hierarchy
#' @export
#' @examples
#' df_hierarchies <- hierarchy_list_as_data_frame(list_graph_hierarchies)
hierarchy_list_as_data_frame <- function(list_graphs){

  df_hierarchies <- do.call(rbind,
                            lapply(list_graphs,
                                   igraph::as_data_frame,
                                   what = "vertices")
                            )
  row.names(df_hierarchies) <- NULL

  return(df_hierarchies)
}

#' Converts a company hierarchy graph to a data frame
#'
#' @param list_graphs The graph containing all the hierarchical company trees
#' @return A data frame with company hierarchy data
#' @keywords graph company hierarchy
#' @export
#' @examples
#' df_hierarchy <- hierarchy_as_data_frame(graph_hierarchy)
hierarchy_as_data_frame <- function(graph){

  df_hierarchy <- igraph::as_data_frame(graph, what = "vertices")

  row.names(df_hierarchy) <- NULL

  return(df_hierarchy)
}

#' Get the sibling company IDs of a company
#'
#' @param graph A graph
#' @param id_company The ID of the company of which you want the sibling IDs
#' @return A vector with company IDs of the siblings
#' @keywords graph company hierarchy
#' @export
#' @examples
#' sibling_ids <- get_sibling_ids(graph, id_company)
get_sibling_ids <- function(graph, id_company){

  # Getting the parent
  id_parent <- names(igraph::ego(graph = graph,
                                    nodes = igraph::V(graph)[id_company],
                                    mode = "out")[[1]])
  id_parent <- id_parent[id_parent != id_company]

  if(length(id_parent) > 0) {
    # Getting the children of those parent
    id_siblings <- names(igraph::ego(graph = graph,
                                        nodes = igraph::V(graph)[id_parent],
                                        mode = "in")[[1]])

    # Excluding the id_company, so only it's siblings are returned
    id_siblings <- id_siblings[id_siblings != id_parent &
                                       id_siblings != id_company]
  } else {
    id_siblings <- character(0)
  }
  return(id_siblings)
}

#' Add a logical variable to the companies in a hierarchy when one of it's
#' attributes match one of a set of values
#'
#' @param graph A graph
#' @param company_ids The name of the newly created logical company attribute
#' @return A data-frame containing all sibling companies
#' @keywords graph company hierarchy
#' @export
#' @examples
#' graph <- get_siblings_df(graph)
get_siblings_df <- function(graph, company_ids){

  tbl_siblings <- data.frame(id_company = as.character(),
                            id_sibling = as.character(),
                            stringsAsFactors = FALSE)

  for(company_ids in company_ids){

    id_sibling <- get_sibling_ids(graph, company_ids)
    id_company <- rep(company_ids, length(id_sibling))
    tbl_siblings <- rbind(tbl_siblings,
                          data.frame(id_company, id_sibling, stringsAsFactors = FALSE))
  }

  # Added stats
  tbl_siblings %<>%
    dplyr::mutate(is_customer = id_sibling %in% company_ids) %>%
    dplyr::filter(!is_customer) %>%
    dplyr::select(-is_customer) %>%
    dplyr::group_by(id_company) %>%
    dplyr::mutate(qty_siblings = n()) %>%
    dplyr::ungroup()

  return(tbl_siblings)
}

#' Get the number of sibling companies
#'
#' @param graph A graph
#' @param vertx The vertex representing a company
#' @return A data-frame containing all sibling companies
#' @keywords graph company hierarchy
#' @examples
#' graph <- get_siblings_df(graph)
get_qty_siblings <- function(graph, vertx) {

  # Getting the parent
  vertx_parent <- igraph::ego(graph = graph,
                              nodes = vertx,
                              mode = "out")[[1]]
  vertx_parent <- vertx_parent[vertx_parent != vertx]

  # Getting the children of those parent
  if(length(vertx_parent) > 0 ){
    vertx_siblings <- igraph::ego(graph = graph,
                                  nodes = igraph::V(graph)[vertx_parent],
                                  mode = "in")[[1]]

    # Excluding the idx_vertex, so only it's siblings are returned
    qty_siblings <- length(vertx_siblings[vertx_siblings != vertx_parent])
  } else {
    qty_siblings <- 0
  }

  return(qty_siblings)
}

#' Add a logical variable to the companies in a hierarchy when one of it's
#' attributes match one of a set of values
#'
#' @param graph A graph
#' @param name_logical The name of the newly created logical company attribute
#' @param name_filter The name of the attribute which values are compared to the criteria
#' @param set_criteria The set of criteria in a vector to which the values are compared
#' @return A graph where all the nodes contain the newly created attribute
#' @keywords graph company hierarchy
#' @export
#' @examples
#' graph <- mark_companies_logical(graph, "is_holding", "code_SBI", c("64", "642", "6420"))
mark_companies_logical <- function(graph, name_logical, name_filter , set_criteria){

  name_filter <- ifelse(name_filter == "id_company", "name", name_filter)

  igraph::vertex_attr(graph, name_logical) <- NA
  igraph::vertex_attr(graph, name_logical) <-
    igraph::vertex_attr(graph, name_filter) %in% set_criteria

  return(graph)
}


#' Add a logical variable to the companies in a hierarchy when one of it's
#' attributes match one of a set of values
#'
#' @param graph A graph
#' @param name_filter The name of the attribute which values are compared to the criteria
#' @param set_criteria The set of criteria in a vector to which the values are compared
#' @return A graph where all the nodes contain the newly created attribute
#' @keywords graph company hierarchy
#' @export
#' @examples
#' count_companies_by_set(graph = graph_company, "code_SBI", "64", "642", "6420"))
count_companies_by_set <- function(graph, name_filter, set_criteria){

  name_filter <- ifelse(name_filter == "id_company", "name", name_filter)
  qty_companies <- sum(igraph::vertex_attr(graph, name_filter) %in% set_criteria)

  return(qty_companies)
}

#' Add hierarchy stats to all the vertices of a company hierarchy graph
#'
#' @param graph A graph
#' @return A graph where all the nodes contain information about the company hierarchy graph
#' @keywords graph company hierarchy
#' @export
#' @examples
#' graph <- add_company_hierarchy_stats(graph)
add_company_hierarchy_stats <- function(graph){

  # Determine company hierarchy is a hierarchical tree
  igraph::vertex_attr(graph, "is_tree") <- igraph::is_dag(graph)
  # Determine the number of companies in the company hierarchies
  igraph::vertex_attr(graph, "qty_hierarchy_companies") <- igraph::vcount(graph)
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
  # Number of sister companues
  igraph::vertex_attr(graph, "qty_sister_companies") <-
    sapply(igraph::V(graph),
           function(x) get_qty_siblings(graph, x))

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
#' @examples
#' graph <- aggregate_hierarchy_value(graph, name_attribute = "qty_employees", name_aggregate = "qty_employees_cum", FUN = sum, na.rm = TRUE)
aggregate_hierarchy_value <- function(graph, name_attribute, name_aggregate, FUN, ...){

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

#' Determine holding SBI replacement
#'
#' @param graph A graph with the company hierarchy
#' @param name_activity_code The name of the value attribute that contains the economic activity code
#' @param vec_holding_codes A vector of codes that represent holdings
#' @return A graph where all the nodes contain aggregated value
#' @keywords graph company hierarchy
#' @export
#' @examples
#' graph <- recode_holding_codes(graph, name_activity_code = "code_sbi", vec_holding_codes = c("64", "642", "6420"))
recode_holding_codes <- function(graph, name_activity_code, vec_holding_codes){

  # Determine the ultimate mother company
  vertx_root <- get_root_vertex(graph)
  # Determine the order of the companies by sorting them in distance from the ultimate mother
  vertex_distances <- igraph::distances(graph,
                                        v = igraph::V(graph),
                                        to = vertx_root)[, 1]
  idx_by_distances <- names(sort(vertex_distances, decreasing = TRUE))

  # Iterate through each company in the network
  for(idx_company in idx_by_distances) {

    # The company vertice
    vertex_company <- igraph::V(graph)[idx_company]

    # Determine wether the node is a holding
    is_holding <- igraph::vertex_attr(graph,
                                      name_activity_code,
                                      index = igraph::V(graph)[idx_company]
    ) %in% vec_holding_codes
    if(is_holding){

      idx_children <- get_incoming_vertice_names(graph, idx_company)
      children_code <- igraph::vertex_attr(graph = graph,
                                           name_activity_code,
                                           index = igraph::V(graph)[idx_children])
      children_code <- children_code[!children_code %in% vec_holding_codes] # Remove holding SBI codes for children
      children_code <- children_code[!is.na(children_code)]                 # Remove empty SBI codes for children

      if(length(children_code) > 0) {

        code_2 <- stringr::str_sub(children_code, 1, 2) # Shorten SBI code to first 2 digits
        freq_2 <- table(code_2)                         # Count SBI code 2-digit occurence
        code_new <- names(freq_2)[which.max(freq_2)][1] # Get first of maximum values
        igraph::vertex_attr(graph,
                            name_activity_code,
                            index = igraph::V(graph)[idx_company]) <- code_new
      }
    }
  }
  return(graph)
}

#' Plots a company hierarchy graph
#'
#' @param graph A graph
#' @param ... The parameters passed to the function specified in FUN
#' @keywords graph company hierarchy
#' @export
#' @examples
#' plot_graydon_graph(graph)
plot_graydon_graph <- function(graph, ...){

  extrafont::loadfonts(device="win", quiet = TRUE)

  igraph::igraph_options(
    vertex.color = col_graydon[4],
    vertex.label.family = "Roboto",
    vertex.label.cex = .7,
    vertex.label.color = col_graydon[8],
    vertex.label.dist = 1,
    vertex.frame.color = col_graydon[3],
    vertex.size = 15,
    edge.color = col_graydon[7],
    edge.arrow.size = 0.5
  )

  igraph::plot.igraph(graph, ...)
}

