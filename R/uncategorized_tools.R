#' Copy a dataframe to the clipboard.
#'
#' @param df The data frame you want to put on the clipboard
#' @param row.names Indicates whether you want to copy the row names as well, default is FALSE.
#' @param col.names Indicates whether you want to copy the column names as well, default is TRUE.
#' @export
#' @examples
#' df_to_clipboard(mtcars)
df_to_clipboard <- function(df, row.names = FALSE, col.names = TRUE, ...) {

  write.table(
    df,
    "clipboard-16384",
    sep = "\t",
    row.names = row.names,
    col.names = col.names,
    dec = ",",
    ...
  )

}

#' Gets the last day of the month of a date in the format YYYY-MM-DD
#'
#' @param date you'll want to get the last month
#' @param tz The time zone indicator, which has a default for the Central European time zone
#' @export
#' @examples
#' end_of_month(as.Date('2018-10-09'), tz = "GMT")
end_of_month <- function(date, tz = "CET") {

  date <- as.POSIXct(date)
  # date character string containing POSIXct date
  date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
  mon <- date.lt$mon + 2
  year <- date.lt$year
  year <- year + as.integer(mon==13) # if month was December add a year
  mon[mon == 13] <- 1
  iso <- ISOdate(1900+year, mon, 1, hour=0, tz = tz)
  result <- as.POSIXct(iso) - 86400 # subtract one day
  result <- result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst) * 3600

  return(result)
}

#' Determining redundant columns of secondary table before joining
#'
#' @param tbl_primary The table of which you
#' @param tbl_secondary The time zone indicator, which has a default for the Central European time zone
#' @param vec_key_columns a vector indicating the column which are the common keys in both tables
#' @export
#' @examples
#' remove_redundant_columns(tbl_primary, tbl_secondary, vec_key_columns)
remove_redundant_columns <- function(tbl_primary, tbl_secondary, vec_key_columns) {

  col_names_both <- c(names(tbl_primary), names(tbl_secondary))
  col_names_duplicate <- col_names[duplicated(col_names)]

  # Exclude join key
  col_names_duplicate <- col_names_duplicate[col_names_duplicate %nin% vec_key_columns]
}

#' Installing regularly used packages
#'
#' @export
#' @examples
#' install_graydon_packages()
install_graydon_packages <- function() {

  # Installing and loading libraries
  list_of_packages <- c("ggplot2", "dplyr", "magrittr", "purrr", "fst", "ggmap", "ggthemes", "reshape2", "scales", "xlsx",
                        "stringr", "RColorBrewer", "qgraph", "Hmisc", "factoextra", "cluster", "kimisc", "ggrepel", "class",
                        "lubridate", "tidyr", "broom", "funr", "htmltools", "outliers", "readr", "janitor", "ggmosaic",
                        "extrafont", "gridExtra", "DT", "formattable", "data.table", "bit64", "igraph")
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]

  if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

  rm(list_of_packages, new_packages)

}
