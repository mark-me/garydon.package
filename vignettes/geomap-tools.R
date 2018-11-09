## ----setup, include = FALSE----------------------------------------------
library(tmap)
library(dplyr)
library(magrittr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(graydon.package)

## ---- warning=FALSE------------------------------------------------------
sp_companies_uk <- create_spatial_df(tbl_companies_uk, lon = "LONGITUDE_RA", lat = "LATITUDE_RA")

## ---- message=FALSE------------------------------------------------------
library(tmap)

tmap_mode("plot")

# Creating the Lieutenancy borders 
tm_shape(sp_uk_lieutenancy, name = "Lieutenancy borders") +
  tm_layout(frame = FALSE) +
  tm_fill(col = col_graydon[4],
          title = "Lieutenancy",
          alpha = .8,
          legend.show = TRUE)  +
  tm_borders(col = "white",
             alpha = 0.2) +
# Adding the company dots  
tm_shape(sp_companies_uk, name = "Companies") +
  tm_layout(aes.color = c(fill = col_graydon[1],
                          borders = col_graydon[1], 
                          symbols = col_graydon[1]),
            frame = FALSE) +
  tm_dots(col = col_graydon[2],
          size = 0.01,
          alpha = 0.7,
          legend.show = FALSE) 

## ---- message=FALSE, warning=FALSE---------------------------------------
tbl_merge <- match_sp(sp_companies_uk, sp_uk_lieutenancy)

## ------------------------------------------------------------------------
tbl_companies_region <- tbl_merge %>% 
  mutate(id_merge = as.character(id_merge)) %>% 
  group_by(id_merge) %>% 
  summarise(qty_companies = n())

## ------------------------------------------------------------------------
sp_uk_lieutenancy@data %<>%
  left_join(tbl_companies_region, by = "id_merge")

## ------------------------------------------------------------------------
tm_shape(sp_uk_lieutenancy, name = "Lieutenancy borders") +
  tm_layout(aes.palette = list(seq = c(col_graydon_low, col_graydon_high)), frame = FALSE) +
  tm_fill(col = "qty_companies",
          title = "Lieutenancy",
          alpha = .8,
          legend.show = TRUE)  +
  tm_borders(col = "white",
             alpha = 0.2)

