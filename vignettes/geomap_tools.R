## ----setup, include = FALSE----------------------------------------------
library(tmap)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(graydon.package)

## ---- warning=FALSE------------------------------------------------------
sp_companies_uk <- create_spatial_df(tbl_companies_uk, lon = "LONGITUDE_RA", lat = "LATITUDE_RA")

