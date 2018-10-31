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

## ------------------------------------------------------------------------
data.frame(`Column names` = names(tbl_company_relations)) %>% 
  knitr::kable()

## ---- message=FALSE, warning=FALSE---------------------------------------
graph_company_hierarchies <- create_graph_company_hierarchies(tbl_company_relations)

## ---- message=FALSE, warning=FALSE---------------------------------------
list_graphs <- list_company_hierarchy_graphs(graph_company_hierarchies)

## ------------------------------------------------------------------------
library(igraph)

## ---- message=FALSE, warning=FALSE---------------------------------------
graph <- list_graphs[[986]]
plot(graph)

## ---- message=FALSE, warning=FALSE---------------------------------------
# Aggregate values
graph <- aggregate_company_hierarchy_value(graph, 
                                           name_attribute = "qty_employees", 
                                           name_aggregate = "qty_employees_cum", 
                                           sum, na.rm=TRUE)

## ---- message=FALSE, warning=FALSE---------------------------------------
V(graph)$label <- paste(V(graph)$name, 
                        V(graph)$qty_employees,
                        V(graph)$qty_employees_cum,
                        sep = " - ")
plot(graph)

## ---- message=FALSE, warning=FALSE---------------------------------------
graph <- add_company_hierarchy_stats(graph)

## ---- message=FALSE, warning=FALSE---------------------------------------
# tbl_hierarchy_set <- hierarchy_code_level(tbl_hierarchy_clean, level_no = 2)

