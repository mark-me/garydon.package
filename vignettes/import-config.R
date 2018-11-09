## ----setup, include = FALSE----------------------------------------------
library(magrittr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

file_col_specs <- "~/uk_companies_columns.csv"
if (file.exists(file_col_specs)) file.remove(file_col_specs) # Delete previous col specs file
rm(file_col_specs)

## ------------------------------------------------------------------------
library(graydon.package)

## ---- echo=FALSE---------------------------------------------------------
data.frame(`Column names` = names(tbl_companies_uk)) %>% 
  knitr::kable()

## ---- message=FALSE, warning=FALSE---------------------------------------
tbl_companies_uk <- apply_column_config(df_source = tbl_companies_uk, 
                                        filename_source = "~/uk_companies.tsv") 

## ---- echo=FALSE---------------------------------------------------------
data.frame(`Column names` = names(tbl_companies_uk)) %>% 
  knitr::kable()

## ---- echo=FALSE---------------------------------------------------------
read.csv2("~/uk_companies_columns.csv") %>% 
  knitr::kable()

## ---- echo=FALSE---------------------------------------------------------
tbl_config <- read.csv2("~/uk_companies_columns.csv", stringsAsFactors = FALSE)

# Change new column name

idx_row <- (tbl_config$original_name == "GDB_ORG_YUID") 
idx_column <- which(names(tbl_config) == "new_name")
tbl_config[idx_row, idx_column] <- "id_graydon"
# Change column inclusion
idx_row <- (tbl_config$original_name %in% c("GB_CRO_REGISTRATION_NUMBER",
                                            "ADDRESS_LINE_1_RA",
                                            "ADDRESS_LINE_2_RA",
                                            "ADDRESS_LINE_3_RA",
                                            "ADDRESS_LINE_4_RA",
                                            "ADDRESS_LINE_5_RA",
                                            "POSTCODE_RA",
                                            "S_EN_STREET_RA",
                                            "S_FR_CITY_RA"))
idx_column <- which(names(tbl_config) == "include")
tbl_config[idx_row, idx_column] <- "FALSE"

write.csv2(tbl_config, "~/uk_companies_columns.csv", row.names = FALSE)

rm(idx_row, idx_column)

tbl_config %>% 
  dplyr::filter(original_name %in% c("GDB_ORG_YUID", 
                                     "GB_CRO_REGISTRATION_NUMBER",
                                     "ADDRESS_LINE_1_RA",
                                     "ADDRESS_LINE_2_RA",
                                     "ADDRESS_LINE_3_RA",
                                     "ADDRESS_LINE_4_RA",
                                     "ADDRESS_LINE_5_RA",
                                     "POSTCODE_RA",
                                     "S_EN_STREET_RA",
                                     "S_FR_CITY_RA")) %>% 
  knitr::kable()

## ----Reset table, echo=FALSE, include=FALSE------------------------------
rm(tbl_config, tbl_companies_uk)

## ---- message=FALSE, warning=FALSE---------------------------------------
tbl_companies_uk <- apply_column_config(df_source = tbl_companies_uk, 
                                        filename_source = "~/uk_companies.tsv") 

## ---- echo=FALSE---------------------------------------------------------
data.frame(`Column names` = names(tbl_companies_uk)) %>% 
  knitr::kable()

## ---- echo=FALSE, include=FALSE------------------------------------------
rm(tbl_companies_uk)

## ---- echo=FALSE, include=FALSE------------------------------------------
# Remove address columns
tbl_companies_uk %<>% 
  dplyr::select(-c("ADDRESS_LINE_1_RA", "ADDRESS_LINE_2_RA", "ADDRESS_LINE_3_RA", "ADDRESS_LINE_4_RA",
                   "ADDRESS_LINE_5_RA", "POSTCODE_RA", "S_EN_STREET_RA", "S_FR_CITY_RA"))

# Add new columns
tbl_companies_uk$ACTIVITY_CODE_2 <- stringr::str_sub(tbl_companies_uk$ACTIVITY_CODE, 1, 2)
tbl_companies_uk$IS_ACTIVE_STATUS <- tbl_companies_uk$IS_ACTIVE_STATUS_CODE == "Y"

## ---- message=FALSE, warning=FALSE---------------------------------------
tbl_companies_uk <- apply_column_config(df_source = tbl_companies_uk, 
                                        filename_source = "~/uk_companies.tsv") 

## ---- echo=FALSE---------------------------------------------------------
data.frame(`Column names` = names(tbl_companies_uk)) %>% 
  knitr::kable()

## ------------------------------------------------------------------------
write_csv2(tbl_companies_uk, path="~/uk_companies_columns.csv")

