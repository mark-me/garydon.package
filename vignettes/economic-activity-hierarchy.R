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

