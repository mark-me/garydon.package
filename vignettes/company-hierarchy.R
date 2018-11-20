## ----setup, include = FALSE----------------------------------------------
library(magrittr)
library(ggplot2)
library(dplyr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(graydon.package)

## ---- echo=FALSE---------------------------------------------------------
data.frame(`Column names` = names(tbl_company_relations)) %>% 
  knitr::kable()

## ---- message=FALSE, warning=FALSE---------------------------------------
library(igraph)

## ---- message=FALSE, warning=FALSE---------------------------------------
graph_company_hierarchies <- create_graph_company_hierarchies(tbl_company_relations)

## ----message=FALSE, warning=FALSE----------------------------------------
plot_graydon_graph(graph_company_hierarchies,
                   vertex.label = "",
                   vertex.size = 4,
                   edge.arrow.size = 0)

## ------------------------------------------------------------------------
id_company_selected <- "931238099"
graph_company_hierarchy <- find_company_hierarchy(graph_company_hierarchies, id_company_selected)

## ---- message=FALSE, warning=FALSE---------------------------------------
igraph::V(graph_company_hierarchy)$color <- ifelse(igraph::V(graph_company_hierarchy)$is_searched_company,
                                                   col_graydon[2],
                                                   col_graydon[4])
plot_graydon_graph(graph_company_hierarchy)

## ---- echo=FALSE---------------------------------------------------------
id_company <- as.character(
  sample(tbl_company_relations$id_company[!is.na(tbl_company_relations$id_company)],
         size = 300)
  )
tbl_customers <- data.frame(id_company, stringsAsFactors = FALSE)

## ------------------------------------------------------------------------
list_selected_hierarchies <- select_graph_hierarchies(graph_company_hierarchies, 
                                                      tbl_customers$id_company)

## ---- echo=FALSE---------------------------------------------------------
qty_customers <- length(unique(tbl_customers$id_company))
qty_graphs <- length(list_selected_hierarchies) 

qty_graph_selected <- sapply(
  lapply(list_selected_hierarchies, 
         igraph::vertex_attr, 
         name = "is_searched_company"
         ), 
  sum
  ) 

id_graph_multiple <- names(qty_graph_selected[qty_graph_selected > 2])
qty_vertices <- sapply(list_selected_hierarchies, igraph::vcount)
id_graph_6_vertices <- names(qty_vertices[qty_vertices > 5])
id_graph_candidates <- id_graph_6_vertices[match(id_graph_6_vertices, id_graph_multiple)]
id_graph_example <- first(id_graph_candidates[!is.na(id_graph_candidates)])
graph_example <- list_selected_hierarchies[[id_graph_example]]
rm(qty_graph_selected, id_graph_multiple, qty_vertices, id_graph_6_vertices, id_graph_example)

## ---- message=FALSE, warning=FALSE---------------------------------------
igraph::V(graph_example)$color <- ifelse(igraph::V(graph_example)$is_searched_company,
                                                   col_graydon[2],
                                                   col_graydon[4])

plot_graydon_graph(graph_example,
                   vertex.label = "")

## ---- message=FALSE, warning=FALSE---------------------------------------
list_all_graphs <- list_company_hierarchy_graphs(graph_company_hierarchies)

## ------------------------------------------------------------------------
df_single_hierarchy <- hierarchy_as_data_frame(graph_company_hierarchy)

## ---- echo=FALSE---------------------------------------------------------
df_single_hierarchy %>% 
  knitr::kable()

## ------------------------------------------------------------------------
df_selected_hierarchies <- hierarchy_list_as_data_frame(list_selected_hierarchies)

## ------------------------------------------------------------------------
graph_company_hierarchy <- 
  aggregate_hierarchy_value(graph = graph_company_hierarchy, 
                            name_attribute = "qty_employees",
                            name_aggregate = "qty_employees_cum",
                            FUN = sum, 
                            na.rm = TRUE)

## ---- message=FALSE, warning=FALSE---------------------------------------
igraph::V(graph_company_hierarchy)$label <- paste0("# ",
                                                   igraph::V(graph_company_hierarchy)$qty_employees,
                                                   " -> Cum # ",
                                                   igraph::V(graph_company_hierarchy)$qty_employees_cum)

plot_graydon_graph(graph_company_hierarchy)

## ------------------------------------------------------------------------
graph_company_hierarchy <- add_company_hierarchy_stats(graph_company_hierarchy)

## ------------------------------------------------------------------------
df_single_hierarchy <- hierarchy_as_data_frame(graph_company_hierarchy)

## ---- echo=FALSE---------------------------------------------------------
df_single_hierarchy %>% 
  select(-color, -label) %>% 
  knitr::kable()

## ------------------------------------------------------------------------
vec_sbi_holdings <- c("64", "642", "6420")

graph_company_hierarchy <- mark_companies_logical(graph_company_hierarchy,
                                                  name_logical = "is_holding",
                                                  name_filter = "code_sbi",
                                                  set_criteria = vec_sbi_holdings)

## ---- warning=FALSE, message=FALSE---------------------------------------
id_siblings <- get_sibling_ids(graph_company_hierarchy, "1003667")

graph_company_hierarchy <- mark_companies_logical(graph_company_hierarchy,
                                                  name_logical = "is_sibling", 
                                                  name_filter = "id_company",
                                                  set_criteria = id_siblings
                                                  )

igraph::V(graph_company_hierarchy)$label <- igraph::V(graph_company_hierarchy)$name
V(graph_company_hierarchy)$color <- ifelse(V(graph_company_hierarchy)$is_sibling,
                                                   col_graydon[2],
                                                   col_graydon[4])

plot_graydon_graph(graph_company_hierarchy)

## ------------------------------------------------------------------------
tbl_siblings <- get_siblings_df(graph_company_hierarchies, tbl_customers$id_company)

## ---- echo=FALSE---------------------------------------------------------
tbl_siblings %>%
  head() %>% 
  knitr::kable()

## ---- message=FALSE, warning=FALSE---------------------------------------
vec_sbi_holdings <- c("64", "642", "6420")

graph_company_hierarchy <- mark_companies_logical(graph_company_hierarchy,
                                                  name_logical = "is_holding",
                                                  name_filter = "code_sbi",
                                                  set_criteria = vec_sbi_holdings)

igraph::V(graph_company_hierarchy)$label <- igraph::V(graph_company_hierarchy)$code_sbi
V(graph_company_hierarchy)$color <- ifelse(V(graph_company_hierarchy)$is_holding,
                                                   col_graydon[2],
                                                   col_graydon[4])

plot_graydon_graph(graph_company_hierarchy)

## ---- message=FALSE, warning=FALSE---------------------------------------
vec_sbi_holdings <- c("64", "642", "6420")
library("png")
 
img_holding <- readPNG("~/R scripts/hierarchy_changes/money-svg-hand-icon-png-3.png")
img_regular <- readPNG("~/R scripts/hierarchy_changes/vector-apartments-business-building-6.png")

graph_company_hierarchy <- mark_companies_logical(graph_company_hierarchy,
                                                  name_logical = "is_holding",
                                                  name_filter = "code_sbi",
                                                  set_criteria = vec_sbi_holdings)

V(graph_company_hierarchy)$raster <- list(img_regular, img_holding)[V(graph_company_hierarchy)$is_holding+1]

plot_graydon_graph(graph_company_hierarchy, 
                   vertex.shape="raster", 
                   vertex.label=NA,
                   vertex.size=24, 
                   vertex.size2=24, 
                   edge.width=2)

## ------------------------------------------------------------------------
count_companies_by_set(graph = graph_company_hierarchy,
                       name_filter = "code_sbi",
                       set_criteria = vec_sbi_holdings)

## ------------------------------------------------------------------------
graph_company_hierarchy <- recode_holding_codes(graph_company_hierarchy, 
                                                name_activity_code = "code_sbi", 
                                                vec_holding_codes = c("64", "642", "6420"))

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
vec_sbi_holdings <- c("64", "642", "6420")

graph_company_hierarchy <- mark_companies_logical(graph_company_hierarchy,
                                                  name_logical = "is_holding",
                                                  name_filter = "code_sbi",
                                                  set_criteria = vec_sbi_holdings)

igraph::V(graph_company_hierarchy)$label <- igraph::V(graph_company_hierarchy)$code_sbi
V(graph_company_hierarchy)$color <- ifelse(V(graph_company_hierarchy)$is_holding,
                                                   col_graydon[2],
                                                   col_graydon[4])

plot_graydon_graph(graph_company_hierarchy)

