library(magrittr)
library(dplyr)
library(graydon.package)

# Function to select one part of the economic hierarchy ----
create_SBI_tree <- function(code_SBI = NA) {

  if(is.na(code_SBI)) {
    level_1 <- (tbl_SBI_count %>% dplyr::filter(hierarchy_layer == 1))$code_SBI
  } else {
    level_1 <- code_SBI
  }
  level_2 <- (tbl_SBI_count %>% dplyr::filter(code_SBI_parent %in% level_1))$code_SBI
  level_3 <- (tbl_SBI_count %>% dplyr::filter(code_SBI_parent %in% level_2))$code_SBI
  level_4 <- (tbl_SBI_count %>% dplyr::filter(code_SBI_parent %in% level_3))$code_SBI
  level_5 <- (tbl_SBI_count %>% dplyr::filter(code_SBI_parent %in% level_4))$code_SBI
  selected_codes <- c(level_1, level_2, level_3, level_4, level_5)
  rm(level_1, level_2, level_3, level_4, level_5)

  tbl_SBI_selected <- tbl_SBI_count %>% dplyr::filter(code_SBI %in% selected_codes)
  rm(selected_codes)

  # Create data frame in right order ----
  tbl_hierarchy <- tbl_SBI_selected %>%
    dplyr::mutate(code_SBI_parent = ifelse(is.na(code_SBI_parent), "0", code_SBI_parent)) %>%
    dplyr::rename(code = code_SBI,
                  code_parent = code_SBI_parent) %>%
    dplyr::select(code, code_parent, dplyr::everything())

  # Creating the graph ----
  graph_tree <- create_economic_activity_graph(tbl_hierarchy)

  # Remove non-sensical 0 vertex ----
  graph_tree <- igraph::delete.vertices(graph_tree, v=get_root_vertex(graph_tree))

  return(graph_tree)
}

# Create SBI graph ----
graph_tree <- create_SBI_tree(c("14"))


plot_graydon_graph(graph_tree)




# Create list of trees for each sector code (letters) ----
list_trees <- igraph::decompose(graph_tree)
lapply(list_trees, plot_econ_hierarchy, title = "Full sector hierarchies", label = "qty_companies_cum")

# Roll up list of trees ----
list_rolled_up <- lapply(list_trees, roll_up_hierarchy_by_minimum,
                         name_attribute = "qty_companies",
                         name_propagated = "qty_companies_cum",
                         threshold = 1000)
lapply(list_rolled_up, plot_econ_hierarchy, title = "Rolled to minimum value", label = "qty_companies_cum")

# Combining graphs in translation table ----
df_rolled <- do.call(rbind, lapply(list_rolled_up, igraph::as_data_frame, what = "edges"))
