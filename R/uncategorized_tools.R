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

#' Getting the list of packages that are regularly used by Tailored Analytics
#'
get_library_names <- function(){
  list_of_packages <- c("ggplot2", "dplyr", "magrittr", "purrr", "ggmap", "ggthemes", "reshape2", "scales", "yaml", "feather",
                        "stringr", "RColorBrewer", "qgraph", "Hmisc", "factoextra", "cluster", "kimisc", "ggrepel", "class",
                        "lubridate", "tidyr", "broom", "funr", "htmltools", "outliers", "readr", "janitor", "ggmosaic", "tictoc",
                        "extrafont", "gridExtra", "DT", "formattable", "data.table", "bit64", "igraph", "rgdal", "tmap", "roxygen2")
  return(list_of_packages)
}

#' Installing regularly used packages
#'
#' @export
#' @examples
#' install_graydon_packages()
install_graydon_packages <- function() {

  # Installing and loading libraries
  list_of_packages <- get_library_names()
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]

  print("Installing packages")
  if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

  rm(list_of_packages, new_packages)

  print("Fonts")
  extrafont::font_import()
}

#' Creates a subdirectory and/or sets the working directory of a project. ----
#'
#' @param project_name The name of the sub directory for the project you use/create
#' @param dir_base The directory where you want to have the project created in. The default is the current working directory
#' @export
#' @examples
#' open_project("My project" , "~/R scripts")
open_project <- function(project_name, dir_base = "~/R scripts") {

  name_project <- project_name

  # Project directory and working directory
  dir_project <<- paste0(dir_base, "/", name_project)
  dir.create(file.path(dir_project), showWarnings = FALSE)
  setwd(dir_project)

  # Set upstream directory - for input data
  dir_upstream <<- paste0("/up/upstream/", name_project)
  dir.create(path = file.path(dir_upstream), showWarnings = FALSE)

  # Set midstream directory - for processed data
  dir_midstream <<- paste0("/mid/midstream/", name_project)
  dir.create(path = file.path(dir_midstream), showWarnings = FALSE)

  # Set downstream directory - for output files like data and reports
  dir_downstream <<- paste0("/down/downstream/", name_project)
  dir.create(path = file.path(dir_downstream), showWarnings = FALSE)

  # Create gitignore
  create_gitignore()

  # Load standard libraries
  libs <- get_library_names()
  lapply(libs, library, character.only = TRUE)
  return(NULL)

}

#' Creates standard gitignore file for a project. ----
create_gitignore <- function(){

  new_gitignore <- FALSE

  if(file.exists(".gitignore")){

    fileConn<-file(".gitignore")
    current_gitignore <- readLines(fileConn)
    new_gitignore <- identical(current_gitignore, c(".Rproj.user", ".Rhistory", ".RData", ".Ruserdata"))

    close(fileConn)
  } else {
    new_gitignore <- TRUE
  }

  if(new_gitignore){
    vec_gitignore <- c("# History files", ".Rhistory", ".Rapp.history")
    vec_gitignore <- c(vec_gitignore, "# Session Data files", ".RData")
    vec_gitignore <- c(vec_gitignore, "# Example code in package build process", "*-Ex.R")
    vec_gitignore <- c(vec_gitignore, "# Output files from R CMD build", "/*.tar.gz")
    vec_gitignore <- c(vec_gitignore, "# Output files from R CMD check", "/*.Rcheck/")
    vec_gitignore <- c(vec_gitignore, "# RStudio files", ".Rproj.user/")
    vec_gitignore <- c(vec_gitignore, "# produced vignettes", "vignettes/*.html", "vignettes/*.pdf")
    vec_gitignore <- c(vec_gitignore, "# OAuth2 token, see https://github.com/hadley/httr/releases/tag/v0.3", ".httr-oauth")
    vec_gitignore <- c(vec_gitignore, "# knitr and R markdown default cache directories",  "/*_cache/", "/cache/")
    vec_gitignore <- c(vec_gitignore, "# Temporary files created by R markdown", "*.utf8.md", "*.knit.md")
    vec_gitignore <- c(vec_gitignore, "# Shiny token, see https://shiny.rstudio.com/articles/shinyapps.html", "rsconnect/", ".Rproj.user")

    fileConn<-file(".gitignore")
    writeLines(vec_gitignore, fileConn)
    close(fileConn)
  }

}
