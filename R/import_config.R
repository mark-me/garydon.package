require(magrittr)

#' A function for applying column configuration file
#'
#' This function applies a column configuration for a source file which
#' you can use to control a data file import .
#' The first time it is used it will create a column specification file.
#' @param df_source The dataframe of which you want to apply/get the import specs for
#' @param filename_source The filename where the data frame is imported fromset
#' @return A data frame containing column importing specs
#' @keywords config import data
#' @export
#' @examples
#' get_columns_file(df, filename_source)
apply_column_config <- function(df_source, filename_source){

  # Getting or creating column specs
  tbl_column_specs <- get_columns_file(df_source, filename_source)

  # Renaming of columns
  names(df_source) <- tbl_column_specs$new_name

  # Selecting included columns
  cols_included <- tbl_column_specs[tbl_column_specs$include, ]$new_name
  df_source <- df_source[, cols_included]

  return(df_source)
}


#' A function for creating/reading data source column specs
#'
#' This function allows you to create and or read a file which you can use to control a data file import
#' @param df_source The dataframe of which you want to apply/get the import specs for
#' @param filename_source The filename where the data frame is imported fromset
#' @return A data frame containing column importing specs
#' @keywords config import data
#' @examples
#' get_columns_file(df, filename_source)
get_columns_file <- function(df_source, filename_source){

  # Generate column file name
  filename_columns <- paste0(gsub("^([^.]*).*", "\\1", filename_source),
                             "_columns.csv")

  # Create column specs, based on passed data frame df_source
  tbl_column_specs <- determine_column_specs(df_source)


  # If there is no column file create one for current data frame
  if(!file.exists(filename_columns)) {

    print(paste("Created column spec file", filename_columns))
    write_colspecs_file(tbl_column_specs, filename_columns)
    return(tbl_column_specs)
  }

  tbl_specs_file <- read_colspecs_file(filename_columns)  # Read column specs from file

  # Check whether the data frame has the same number of columns
  equal_column_specs <- has_equal_column_specs(tbl_column_specs, tbl_specs_file)

  if(equal_column_specs){
    # If the column specs are equal, take column file
    print(paste("Current column spec file fits current source", filename_columns))
    tbl_column_specs <- tbl_specs_file
    return(tbl_column_specs)
  }

  # See wether there are any similarities between the just read source and the column file
  has_similar_columns <- nrow(tbl_column_specs %>%
                                dplyr::inner_join(tbl_specs_file, by = "original_name")) > 0

  # Keep file values for source colums with the same name as the original
  if(has_similar_columns){

    print(paste("Changed current column spec file to fit new source", filename_columns))
    tbl_column_specs %<>%
      dplyr::left_join(tbl_specs_file, by = "original_name")

    # Make sure previously saved column specs are used
    tbl_column_specs %<>%
      dplyr::mutate(include = ifelse(is.na(include.y), include.x, include.y)) %>%
      dplyr::mutate(data_type = ifelse(is.na(data_type.y), data_type.x, data_type.y)) %>%
      dplyr::mutate(new_name = ifelse(is.na(new_name.y), new_name.x, new_name.y)) %>%
      dplyr::mutate(date_added = dplyr::if_else(is.na(date_added.y), date_added.x, date_added.y)) %>%
      dplyr::select(original_name, data_type, new_name, include, date_added)
  }

  # if there are no similarities create
  write_colspecs_file(tbl_column_specs, filename_columns)

  return(tbl_column_specs)
}

#' A function to determine column specs for a data frame
#'
#' This function allows you to read data frame specs into a data frame
#' @param df_source The dataframe of which you want to read the column specs from
#' @return A data frame containing the column specs of df_source
#' @keywords config import data
#' @examples
#' determine_column_specs(df)
determine_column_specs <- function(df_source){

  col_names_orig <- names(df_source)               # Column names
  col_includes <- rep(TRUE, length(col_names_orig)) # Remove columns from source
  col_types <- sapply(df_source, typeof)           # Column types
  date_added <- format(Sys.time(), "%Y-%m-%d %X")   # Current date time

  # Create column file table
  tbl_column_specs <- data.frame(col_names_orig,
                                 col_types,
                                 col_names_orig,
                                 col_includes,
                                 date_added,
                                 stringsAsFactors = FALSE)
  names(tbl_column_specs) <- c("original_name", "data_type", "new_name", "include", "date_added")

  tbl_column_specs$date_added <- as.POSIXct(tbl_column_specs$date_added)

  tbl_column_specs$new_name <- clean_column_names(tbl_column_specs$new_name)

  return(tbl_column_specs)
}

#' Test whether two column specs are equal
#'
#' This function allows you to read data frame specs into a data frame
#' @param tbl_column_specs_a The dataframe with column specs
#' @param tbl_column_specs_b The dataframe with column specs
#' @return A boolean indicating whether the two column specs are equal
#' @keywords config import data
#' @examples
#' has_equal_column_specs(df_specs_a, df_specs_b)
has_equal_column_specs <- function(tbl_column_specs_a, tbl_column_specs_b){

  tbl_column_specs_a %<>% dplyr::select(-date_added, -include, -new_name)
  tbl_column_specs_b %<>% dplyr::select(-date_added, -include, -new_name)

  return(
    isTRUE(dplyr::all_equal(tbl_column_specs_a,
                            tbl_column_specs_b,
                            ignore_col_order = TRUE,
                            ignore_row_order = TRUE))
  )
}

#' Clean column names
#'
#' This function allows you to read data frame specs into a data frame
#'
#' @param column_names a vector with column
#' @return A boolean indicating whether the two column specs are equal
#' @keywords config import data
#' @examples
#' clean_column_names(column_names)
clean_column_names <- function(column_names){

  # New column name cleaning
  column_names <- trimws(tolower(column_names))
  column_names <- gsub('([[:punct:]])|\\s+', '_', column_names)

  # Rename 'code' columns
  code_parts <- c("code")
  column_names <- auto_rename_columns(column_names,
                                      detection_words = code_parts,
                                      prefix = 'code_')
  # Rename 'date' columns
  date_parts <- c("datum", "date")
  column_names <- auto_rename_columns(column_names,
                                      detection_words = date_parts,
                                      prefix = 'date_')

  return(column_names)
}

#' Autorenames columns based on word detection and adding a prefix
#'
#' @param column_names a vector with column
#' @param detection_words The words that indicate a prefix should be added to the column name
#' @param prefix The prefix the should be added in case a word is detected
#' @return A boolean indicating whether the two column specs are equal
#' @keywords config import data
#' @examples
#' clean_column_names(column_names)
auto_rename_columns <- function(column_names, detection_words, prefix) {

  regex_detection_words <- paste0('(', paste(detection_words, collapse = "|"), ')')
  idx_detected <- stringr::str_detect(column_names, regex_detection_words)

  # Removing the detected word
  column_names[idx_detected] <- gsub(regex_detection_words, '', column_names[idx_detected])
  # Add the prefix when a word is detected
  column_names[idx_detected] <- paste0(prefix, column_names[idx_detected])
  # Clean up after detected word removal like _ remainders
  column_names[idx_detected] <- gsub('_$', '', column_names[idx_detected])
  column_names[idx_detected] <- gsub('__', '_', column_names[idx_detected])

  return(column_names)
}

#' Read column specs from a file
#'
#' @param filename_columns The file name containing the column specifications
#' @return A data frame containing the column specifications
#' @keywords config import data
#' @examples
#' tbl_column_specs <- read_colspecs_file(filename_specs)
read_colspecs_file <- function(filename_columns){

  col_types <- readr::cols(
    original_name = readr::col_character(),
    data_type = readr::col_character(),
    new_name = readr::col_character(),
    include = readr::col_logical(),
    date_added = readr::col_datetime(format = '%Y-%m-%d %X')
  )

  # Read column specs from file
  sink("aux")
  tbl_specs_file <- readr::read_csv2(filename_columns,
                                     col_types = col_types)
  sink()

  return(tbl_specs_file)
}

#' Write column specs to file
#'
#' @param tbl_col_specs A data-frame containing the column specifications
#' @param filename_columns The file to write the column specifications to
#' @keywords config import data
#' @examples
#' tbl_column_specs <- read_colspecs_file(filename_specs)
write_colspecs_file <- function(tbl_col_specs, filename_columns){

  tbl_col_specs %<>%
    dplyr::mutate(date_added = format(date_added))

  write_csv2(tbl_col_specs, filename_columns)
}

#' A function saving European-style CSV-files
#'
#' The write_csv2 function that is missing from the readr package
#' The first time it is used it will create a column specification file
#' allows you to create and or read a file which you can use to control a data file import
#' @param x A data frame to write to disk
#' @param path Path or connection to write to.
#' @param na String used for missing values. Defaults to NA. Missing values will never be quoted; strings with the same value as na will always be quoted.
#' @param append If `FALSE`, will overwrite existing file. If `TRUE`, will append to existing file. In both cases, if file does not exist a new file is created.
#' @param col_names Write columns names at the top of the file?
#' @keywords config import data
#' @export
#' @examples
#' write_csv2(tbl_companies_uk, path="~/uk_companies_columns.csv")
write_csv2 <- function(x, path, na = "NA", append = FALSE, col_names = !append) {

  stopifnot(is.data.frame(x))

  numeric_cols <- vapply(x,
                         is.numeric,
                         logical(1))
  x[numeric_cols] <- lapply(x[numeric_cols],
                            format,
                            decimal.mark = ",")
  readr::write_delim(x,
                     path,
                     delim = ";",
                     na = na,
                     append = append,
                     col_names = col_names)
}
