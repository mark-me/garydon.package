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
data.frame(`Column names` = names(tbl_SBI_count)) %>% 
  knitr::kable()

## ---- message=FALSE, warning=FALSE---------------------------------------
tbl_hierarchy_clean <- clean_hierarchy(tbl_SBI_count,
                                       col_code = "code_SBI",
                                       col_code_parent = "code_SBI_parent",
                                       col_layer_no = "hierarchy_layer",
                                       col_value = "qty_companies")

## ---- message=FALSE, warning=FALSE---------------------------------------
plot_hierarchy(tbl_hierarchy_clean, title = "Original",
               col_code = "code_SBI",
               col_code_parent = "code_SBI_parent",
               col_layer_no = "hierarchy_layer",
               col_value = "qty_companies")

## ---- message=FALSE, warning=FALSE---------------------------------------
# Rolling up NACE hierarchy
# tbl_translation <- roll_up_hierarchy(tbl_hierarchy_clean, 100000)

## ---- message=FALSE, warning=FALSE---------------------------------------
# Pushing back numbers to the complete NACE hierarchy ----
# tbl_hierarchy_rolled <- tbl_hierarchy_clean %>% 
#   select(-value) %>% 
#   left_join(tbl_translation, by = c("code" = "code_new")) %>% 
#   group_by(code, code_parent, layer_no) %>% 
#   summarise(value = sum(value, na.rm = TRUE)) %>% 
#   ungroup()

## ---- message=FALSE, warning=FALSE---------------------------------------
# Cleaning hierarchy by removing codes that have a 0 value and are non connective (don't have children that contain values)
# tbl_hierarchy_rolled <- clean_hierarchy(tbl_hierarchy_rolled)

## ---- message=FALSE, warning=FALSE---------------------------------------
# plot_hierarchy(tbl_hierarchy_rolled, title = "Rolled up")

## ---- message=FALSE, warning=FALSE---------------------------------------
# tbl_hierarchy_set <- hierarchy_code_level(tbl_hierarchy_clean, level_no = 2)

