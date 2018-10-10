#' Determine which economic activity definition level is optimal if you want to have a minimum value in a economic activity group
#'
#' This function can be used to aggregate a NACE or SBI code economic activity tree,
#' so the codes represent enough of something for example number of companies,
#' number of customers or revenue.
#'
#' @param tbl_hierarchy  a data frame that should contain the entire NACE or SBI hierarchy with a
#' quantity or amount. It should contain the following columns: \describe{
#'   \item{code}{a NACE or SBI code.}
#'   \item{code_parent}{a NACE or SBI code that refers to the direct parent code.}
#'   \item{layer_no}{an integer indicating the hierarchy's level, 1 being the top level}
#'   \item{value}{the quantity that is used to evaluate whether the code should be kept in
#'   place or should be 'pushed up' the hierarchy.}
#' }
#'
#' @param threshold The minimum value that the tbl_hierarchy value column should have to leave a
#' code in it's place.
#' @return a data frame containing \describe{
#'   \item{code}{The original code which was input.}
#'   \item{code_new}{The replacement code, which indicates the new position in the hierarchy}
#'   \item{value}{The value associated with the original code (input)}
#' }
#' @keywords SBI NACE SIC
#' @export
#' @examples
#' roll_up_hierarchy(tbl_hierarchy, threshold = 200)
roll_up_hierarchy <- function(tbl_hierarchy, threshold) {

  # End-points of the roll-up
  tbl_hierarchy_aggr <- data.frame(code = as.character(),
                                   code_new = as.character(),
                                   value = as.integer())

  # Codes that are pushed up the tree (code has too low value to be end-point)
  tbl_level_migrating <- data.frame(code = as.character(),
                                    code_parent = as.character(),
                                    value = as.integer())

  # Iterator for the levels in the hierarchy, from bottom to top
  levels <- sort(unique(tbl_hierarchy$layer_no), decreasing = TRUE)

  for (i in levels[-length(levels)]) {

    # Get the current level
    tbl_level <- tbl_hierarchy %>%
      dplyr::filter(layer_no == i)  %>%
      dplyr::mutate(code_orig = code) %>%
      dplyr::select(code, code_parent, code_orig, value)

    # Make previous level codes have parent code of their parents
    tbl_level_prev <- tbl_level %>%
      dplyr::select(-value, -code_orig) %>%
      dplyr::inner_join(tbl_level_migrating, by = c("code" = "code_parent")) %>%
      dplyr::rename(code_orig = code.y)

    # Calculate this level's and previous level values per code
    tbl_up_or_down <- rbind(tbl_level, tbl_level_prev) %>%
      dplyr::group_by(code, code_parent) %>%
      dplyr::mutate(value_code = sum(value, na.rm = TRUE)) %>%
      dplyr::ungroup()

    # Identify the codes that have a sufficient quantity to stay at the current level
    tbl_stays <- tbl_up_or_down %>%
      dplyr::filter(value_code >= threshold & value > 0) %>%
      dplyr::mutate(code_new = code,
                    code = code_orig) %>%
      dplyr::select(code, code_new, value)

    # Add the codes that stay behind to the result table
    tbl_hierarchy_aggr <- rbind(tbl_hierarchy_aggr, tbl_stays)

    # Gather the codes that need to be moved up further
    tbl_level_migrating <- tbl_up_or_down %>%
      dplyr::filter(value > 0 & value_code < threshold & value_code > 0) %>%
      dplyr::mutate(code = code_orig) %>%
      dplyr::select(code, code_parent, value)

  }

  # Handling the top level
  # Get the current level
  tbl_level <- tbl_hierarchy %>%
    dplyr::filter(layer_no == i - 1 & value > 0)  %>%
    dplyr::mutate(code_new = code) %>%
    dplyr::select(code, code_new, value)

  # Place all data that hasn't been placed yet on the top level
  tbl_remainder <- rbind(tbl_level,
                         tbl_level_migrating %>%
                           dplyr::rename(code_new = code_parent))

  tbl_hierarchy_aggr <- rbind(tbl_hierarchy_aggr, tbl_remainder)

}

#' A function for enriching each of the codes from it's chosen level's code value
#'
#' @param tbl_hierarchy  a data frame that should contain the entire NACE or SBI hierarchy with a
#' quantity or amount. It should contain the following columns: \describe{
#'   \item{code}{a NACE or SBI code.}
#'   \item{code_parent}{a NACE or SBI code that refers to the direct parent code.}
#'   \item{layer_no}{an integer indicating the hierarchy's level, 1 being the top level}
#'   \item{value}{the quantity that is used to evaluate whether the code should be kept in
#'   place or should be 'pushed up' the hierarchy.}
#' }
#' @param level_no
#' @param tbl_hierarchy  a data frame that should contain the entire NACE or SBI hierarchy with
#' the added code from the requested level: \describe{
#'   \item{code}{a NACE or SBI code.}
#'   \item{code_parent}{a NACE or SBI code that refers to the direct parent code.}
#'   \item{layer_no}{an integer indicating the hierarchy's level, 1 being the top level}
#'   \item{code_level_x}{The code at the chosen level of the hierarchy}
#' }
#' @keywords SBI NACE SIC
#' @export
#' @examples
#' hierarchy_code_level(tbl_hierarchy, level_no = 2)
hierarchy_code_level <- function(tbl_hierarchy, level_no){

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
  names(tbl_level_stored) <- replace(
    names(tbl_level_stored),
    names(tbl_level_stored) == "code_parent",
    paste0("code_level_", as.character(level_no))
  )

  # Add code back to original data frame
  tbl_hierarchy %<>%
    dplyr::left_join(tbl_level_stored, by = "code")

  return(tbl_hierarchy)
}


#' Enrich each of the codes with all of the codes from the levels above the current code
#'
#' @param tbl_hierarchy  a data frame that should contain the entire NACE or SBI hierarchy with a
#' quantity or amount. It should contain the following columns: \describe{
#'   \item{code}{a NACE or SBI code.}
#'   \item{code_parent}{a NACE or SBI code that refers to the direct parent code.}
#'   \item{layer_no}{an integer indicating the hierarchy's level, 1 being the top level}
#' }
#' @return a data frame containing the complete tbl_hierarchy data frame, adding the column ode_level_x (the code at the chosen level of the hierarchy)
#' @keywords SBI NACE SIC
#' @export
#' @example
#' tbl_complete_sbi <- hierarchy_code_all(tbl_sbi_count)
hierarchy_code_all <- function(tbl_hierarchy){

  # Iterator for the levels in the hierarchy, from top to specified level
  levels <- sort(unique(tbl_hierarchy$layer_no))
  for (i in levels[-1]) {
    tbl_hierarchy <- hierarchy_code_level(tbl_hierarchy, level_no = i)
  }
  return(tbl_hierarchy)
}

#' cleans up all nace codes from the hierarchy that contain a NA value and are non-connective
#'
#' @param tbl_hierarchy  a data frame that should contain the entire NACE or SBI hierarchy with a
#' quantity or amount. It should contain the following columns: \describe{
#'   \item{code}{a NACE or SBI code.}
#'   \item{code_parent}{a NACE or SBI code that refers to the direct parent code.}
#'   \item{layer_no}{an integer indicating the hierarchy's level, 1 being the top level}
#'   \item{value}{the quantity that is used to evaluate whether the code should be kept in
#'   place or should be 'pushed up' the hierarchy.}
#' }
#' @return a data frame containing only the codes with other values than NA and are non-connecting
#' @keywords SBI NACE SIC
#' @export
#' @example
#' tbl_sbi_clean <- clean_hierarchy(tbl_sbi_count)
clean_hierarchy <- function(tbl_hierarchy) {

  # Iterator for the levels in the hierarchy, from bottom to top
  levels <- sort(unique(tbl_hierarchy$layer_no), decreasing = TRUE)

  # Codes that have quantities of connective codes
  tbl_level_codes <- data.frame(code = as.character(),
                                code_parent = as.character(),
                                layer_no = as.integer(),
                                value = as.numeric(),
                                has_child = as.logical())

  for (i in levels) {

    # Get the current level
    tbl_level <- tbl_hierarchy %>%
      dplyr::filter(layer_no == i)  %>%
      dplyr::select(code, code_parent, layer_no, value)

    # Joing previous layer to see whether codes are connective
    tbl_level_prev <- tbl_level %>%
      dplyr::left_join(tbl_level_codes, by = c("code" = "code_parent")) %>%
      dplyr::mutate(layer_no = i,
                    has_child = !is.na(code.y)) %>%
      dplyr::rename(value = value.x) %>%
      dplyr::group_by(code, code_parent, layer_no, value) %>%
      dplyr::summarise(has_child = max(has_child)) %>%
      dplyr::ungroup()

    # Remove codes that have no value and are not connective
    tbl_level_prev %<>%
      dplyr::filter(has_child == 1 | (!is.na(value) & value != 0))

    tbl_level_codes <- rbind(tbl_level_codes, tbl_level_prev)
  }
  return(tbl_level_codes)
}

#' Plotting hierarchies based on a single value, mostly for example puroposes
#'
#' @param tbl_hierarchy  a data frame that should contain the entire NACE or SBI hierarchy with a
#' quantity or amount. It should contain the following columns: \describe{
#'   \item{code}{a NACE or SBI code.}
#'   \item{code_parent}{a NACE or SBI code that refers to the direct parent code.}
#'   \item{layer_no}{an integer indicating the hierarchy's level, 1 being the top level}
#'   \item{value}{the quantity that is used to evaluate whether the code should be kept in
#'   place or should be 'pushed up' the hierarchy.}
#' }
#' @keywords SBI NACE SIC
#' @export
#' @example
#' plot_hierarchy(tbl_sbi_count)
plot_hierarchy <- function(tbl_hierarchy, title = ""){

  library(igraph)

  # Create graph
  tbl_nodes <- data.frame(code = with(tbl_hierarchy, unique(c(code, code_parent)))) %>%
    dplyr::left_join(tbl_hierarchy, by = "code") %>%
    dplyr::mutate(value = ifelse(is.na(value), 0, value),
                  layer_no = ifelse(is.na(layer_no), 0, layer_no),
                  layer_no = factor(layer_no))

  tbl_links <- tbl_hierarchy %>% select(code_parent, code, everything())

  graph_hierarchy <- graph_from_data_frame(tbl_links, tbl_nodes, directed = TRUE)

  # Layout graph
  graph_layout <- layout_as_tree(graph_hierarchy, circular = TRUE)
  V(graph_hierarchy)$color <- as.integer(V(graph_hierarchy)$layer_no)
  V(graph_hierarchy)$size <- 5
  V(graph_hierarchy)$label <- ""
  V(graph_hierarchy)$label.family <- "Roboto"
  E(graph_hierarchy)$arrow.size <- 0

  p_hierarchy <- plot(graph_hierarchy,
                      palette = col_graydon,
                      main = title,
                      layout = graph_layout)
  # # Create Graph
  # set.seed(42)
  # p_hierarchy <- ggraph(graph, 'dendrogram', circular = TRUE) +
  #   geom_edge_diagonal(edge_width = 0.5, alpha = .4) +
  #   geom_node_point(aes(colour = layer_no, size = value),
  #                   alpha = 0.4) +
  #   guides(col = FALSE, size = FALSE) +
  #   labs(title = title)
  #
  rm(graph, tbl_nodes, tbl_links)
  return(p_hierarchy)
}
