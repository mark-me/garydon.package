## ----setup, include = FALSE----------------------------------------------
library(magrittr)
library(ggplot2)
library(dplyr)
library(igraph)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(graydon.package)

## ------------------------------------------------------------------------
data.frame(`Column names` = names(tbl_SBI_count)) %>% 
  knitr::kable()

## ---- message=FALSE, warning=FALSE---------------------------------------
graph_SBI <- create_economic_activity_graph(tbl_SBI_count, 
                                            col_id = "code_SBI", 
                                            col_id_parent = "code_SBI_parent")

## ---- message=FALSE, warning=FALSE, fig.height=6, fig.width=6------------
plot_graydon_graph(graph_SBI, 
                   vertex.label = "", 
                   vertex.size = 3, 
                   edge.arrow.size = 0)

## ---- message=FALSE, warning=FALSE---------------------------------------
# graph_SBI_rolled <- roll_up_hierarchy_by_minimum(graph_tree = graph_SBI, 
#                                                  name_attribute = "qty_companies", 
#                                                  name_propagated = "qty_companies_cum", 
#                                                  threshold = 5000)

## ---- message=FALSE, warning=FALSE, fig.height=6, fig.width=6------------
# V(graph_SBI_rolled)$color <- ifelse(V(graph_SBI_rolled)$is_root, 1, 2)
# V(graph_SBI_rolled)$label <- ifelse(V(graph_SBI_rolled)$is_root, V(graph_SBI_rolled)$name, "")
# 
# plot_graydon_graph(graph_SBI_rolled, 
#                    #vertex.label = "", 
#                    vertex.size = 3, 
#                    edge.arrow.size = 0)

## ------------------------------------------------------------------------
# list_graphs <- decompose(graph_SBI_rolled)
# 
# idx_searched <- names(sapply(list_graphs, function(x) igraph::V(x)[1] ))
# idx_searched 
# 
# igraph::V(list_graphs[[3]])[1]
# plot_graydon_graph(list_graphs[[1]])

## ------------------------------------------------------------------------
# df_translation_codes <- hierarchy_as_data_frame(graph_SBI_rolled)

