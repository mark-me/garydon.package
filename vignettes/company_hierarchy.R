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

