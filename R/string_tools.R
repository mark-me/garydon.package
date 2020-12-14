#' Sets first letter of a string to capital
#'
#' @param x The string you want to apply the formatting to
#' @return A formatted string
#' @export
#' @examples
#' str_firstup("ThIs iS One uGLy sTRIng!")
str_firstup <- function(x) {

  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

#' Wraps texts using hyphenation
#'
#' @param text The text you want to wrap
#' @param max_length The maximum length a string may have
#' @param html_format Indicate whether you want <br> new lines
#' @return String wrapped with newline characters
#' @export
#' @examples
#' str_wrap_hyphenate(tbl_SBI$description_SBI, 25)
str_wrap_hyphenate <- function(text, max_length, html_format = FALSE){

  text_wrapped <- stringr::str_wrap(text, max_length)
  text_cleaned <- stringr::str_remove_all(text_wrapped, "- ")
  text_cleaned <- stringr::str_replace_all(text_cleaned, " -\n", "\n")

  if(html_format){
    text_cleaned <- stringr::str_replace_all(text_cleaned, "\n", "<br>")
  }

  return(text_cleaned)
}

#' Sets first letter of a string to capital
#'
#' @param x The string you want to apply the formatting to
#' @param number_decimals The number of decimals you want to display (default = 0)
#' @param format_EN use EN notation; meaning '.' a decimal and ',' as thousands seperators.
#' @param scale Used when you want to format the amount in scales (regular, per thousands or millions)
#' @return A formatted string
#' @export
#' @examples
#' format_number(1000000.01)
format_number <- function(x, number_decimals = 0, format_EN = FALSE, scale = c("normal", "k", "M")) {

  big_mark <- '.'
  decimal_mark <- ','

  if(format_EN){
    big_mark <- ','
    decimal_mark <- '.'
  }

  # Apply scale
  scale <- ifelse(length(scale) > 1, "normal", scale)

  if (scale == "normal") {
    suffix <- ""
  }
  else if (scale == "k") {
    suffix <- "k"
    x <- x / 1000
    number_decimals <- ifelse(is.na(number_decimals), 1, number_decimals)
  }
  else if (scale == "M") {
    suffix <- ifelse(format_EN, "M", " Mln.")
    number_decimals <- ifelse(is.na(number_decimals), 1, number_decimals)
    x <- x / (10 ^ 6)
  }

  # Apply numeric seperators
  number_formatted <- formatC(
    x,
    format = "f",
    big.mark = big_mark,
    decimal.mark = decimal_mark,
    digits = number_decimals
  )

  return(paste0(number_formatted, suffix))
}

#' Formats a number into a currency string
#'
#' @param amount The number you want to format as euros
#' @param currency The currency symbol that should be used as a prefix
#' @param number_decimals The largest number that should still be formatted using cents
#' @param scale Used when you want to format the number in scales (regular, per thousands or millions)
#' @return A string containing the euro formatted number
#' @export
#' @examples
#' format_currency(amount = 1000000, currency = "EUR", number_decimals = 1,scale = "M")
format_currency <- function(amount,
                            currency = c("EUR", "GBP"),
                            number_decimals = 2,
                            scale = c("normal", "k", "M")) {
  scale <- ifelse(length(scale) > 1, "normal", scale)
  amount <- round(amount, digits = 2)

  currency_symbol <- intToUtf8(8364) # Set default currency symbol (EURO)

  # Determine number format (seperators)
  if(currency == "EUR") {
    currency_symbol <- "\u20ac"
    format_EN <- FALSE
  } else if (currency == "GBP") {
    currency_symbol <- "\ua3"
    format_EN <- TRUE
  }

  formatted_number <- format_number(x = amount,
                                    number_decimals = number_decimals,
                                    format_EN = format_EN,
                                    scale = scale)
  return(paste0(currency_symbol, formatted_number))
}

#' Formats a number to percentage by adding "." for thousands, "," for decimals and "\%" to the number
#'
#' @param percentage The number you want to format
#' @param number_decimals The number of decimal places you want shown. Default is 1
#' @param format_EN Indicate whether the formatting should be done UK style. Default is FALSE
#' @return A string containing the formatted number
#' @export
#' @examples
#' format_percent(percentage = 0.123, number_decimals = 1, format_EN = TRUE)
format_percent <- function(percentage, number_decimals = 1, format_EN = FALSE) {

  percentage <- percentage * 100

  percentage <- format_number(x = percentage,
                              number_decimals = number_decimals,
                              format_EN = format_EN,
                              scale = "normal")

  percentage <- paste0(percentage, "%")
  return(percentage)
}

#' Returns a string containing a specified number of characters from a string
#'
#' @param text The string you want the get a piece of
#' @param idx_start  Character position in string at which the part to be taken begins. If start is greater than the number of characters in string, NA will be returned.
#' @param qty_characters If omitted or if there are fewer than length characters in the text (including the character at start), all characters from the start position to the end of the string are returned.
#' @return A string
#' @export
#' @examples
#' str_mid(text = "A long, long time ago", idx_start = 89)
#' str_mid(text = "A long, long time ago", idx_start = 9, qty_characters = 9)
str_mid <- function(text, idx_start, qty_characters = NA) {

  qty_total <- stringr::str_length(text)

  if(idx_start > qty_total){
    return(NA)
  }

  if(!is.na(qty_characters)){
    idx_end <- idx_start + qty_characters - 1
  } else {
    idx_end <- qty_total
  }

  return(substr(text, idx_start, idx_end))
}

#' Extracts a substring from a string, starting from the right-most character
#'
#' @param text The string you want the get a piece of
#' @param qty_characters If  the number of characters that you wish to extract starting from the left-most character.
#' @return A string
#' @export
#' @examples
#' str_right(text = "A long, long time ago", qty_characters = 9)
str_right <- function(text, qty_characters) {

  qty_total <- stringr::str_length(text)

  if(qty_characters > qty_total){
    return(NA)
  }

  idx_start <- qty_total - qty_characters + 1

  return(substr(text, idx_start, qty_total))
}

#' Creates a vector of human readable ntiles
#'
#' @param vec_values The vector/column of values you want to 'ntile'
#' @param qty_ntiles The number of ntiles you want to have
#' @param use_intervals Do you want the labels to contain the value intervals or the ntile naming?
#' @param format_EN Determines whether the named ntiles are English or Dutch.
#' @param FUN The function you want to use to apply formatting
#' @param ... The parameters you'd want to pass on to the formatting function supplied to FUN
#' @return Ordered factor
#' @export
#' @examples
#' ntiles_labeled(vec_values = mtcars$mpg, qty_ntiles = 4, FUN = format_currency, currency = "GBP")
ntiles_labeled <- function(values, qty_ntiles, use_intervals = FALSE, format_EN = FALSE, FUN, ...) {

  ntile_values <- dplyr::ntile(values, qty_ntiles) # Create n-tiles
  ntile_values <- ordered(ntile_values)            # Turn then into ordered vectors

  if(use_intervals) {
    # Using intervals as labels
    ntile_value_min <- ntile_value_max <- NULL
    # Search for minimum and maximum value of each interval
    for(ntile in levels(ntile_values)){

      # Finding minimum value
      ntile_value_min <- c(ntile_value_min, min(values[which(ntile_values == ntile)]))
      # Finding maximum value
      ntile_value_max <- c(ntile_value_max, max(values[which(ntile_values == ntile)]))
    }
    # Format the minimum and maximum values
    ntile_value_min <- sapply(X=ntile_value_min, FUN=FUN, ...)
    ntile_value_max <- sapply(X=ntile_value_max, FUN=FUN, ...)

    # Reassembling the levels, but now formatted
    ntile_levels_new <- paste0("[", ntile_value_min, " - ", ntile_value_max, "]")
    levels(ntile_values) <- ntile_levels_new

  } else {
    # Using ordinal suffixes as labels
    if(format_EN){ levels(ntile_values) <- scales::ordinal(ntile_values)
    } else { levels(ntile_values) <- paste0(levels(ntile_values), "e") }

  }

  return(ntile_values)
}

#' Creates a vector of human readable bins based on width
#'
#' @param values The vector/column of values you want to 'ntile'
#' @param width The width of the bins
#' @param FUN The function you want to use to apply formatting
#' @param ... The parameters you'd want to pass on to the formatting function supplied to FUN
#' @return Ordered factor
#' @export
#' @examples
#' bin_width_labeled(values = mtcars$mpg, qty_ntiles = 4, FUN = format_currency, currency = "GBP")
bin_width_labeled <- function(values, width, boundary=0, FUN, ...) {

  bin_values <- ggplot2::cut_width(values, width, boundary=boundary) # Create bins
  bin_labels <- label_intervals(values, bin_values, FUN, ...)

  return(bin_labels)
}

#' Creates a vector of human readable bins based by specifying the number of bins in advance
#'
#' @param values The vector/column of values you want to bin
#' @param qty_bins The number of ntiles you want to have
#' @param FUN The function you want to use to apply formatting
#' @param ... The parameters you'd want to pass on to the formatting function supplied to FUN
#' @return Ordered factor
#' @export
#' @examples
#' bin_quantity_labeled(values = mtcars$mpg, qty_ntiles = 4, FUN = format_currency, currency = "GBP")
bin_quantity_labeled <- function(values, qty_bins, boundary=0, FUN, ...) {

  bin_values <- ggplot2::cut_interval(values, n = qty_bins, boundary=boundary) # Create bins
  bin_labels <- label_intervals(values, bin_values, FUN, ...)

  return(bin_labels)
}

#' Creates a vector of human readable bins
#'
#' @param values The original values
#' @param intervals The created intervals
#' @param FUN The function you want to use to apply formatting
#' @param ... The parameters you'd want to pass on to the formatting function supplied to FUN
#' @return Ordered factor
#' @examples
#' label_intervals(vec_values = mtcars$mpg, intervals, FUN = format_currency, currency = "GBP")
label_intervals <- function(values, intervals, FUN, ...){

  # Decomposing the interval levels
  bin_levels <- levels(intervals)
  bin_lower_inclusive <- grepl("\\[", as.character(bin_levels)) # Determine whether the lower limit is inclusive
  bin_upper_inclusive <- grepl("\\]", as.character(bin_levels)) # Determine whether the upper limit is inclusive
  # Get lower and upper boundary values
  first_last <- strsplit( gsub("\\(|\\[|\\]|\\)", "", bin_levels), split = ",")
  bin_value_min <- as.double(sapply(first_last, `[[`, 1))
  bin_value_max <- as.double(sapply(first_last, `[[`, 2))
  rm(first_last)

  # Reformatting lower and upper boundary values
  bin_value_min <- sapply(X=bin_value_min, FUN=FUN, ...)
  bin_value_max <- sapply(X=bin_value_max, FUN=FUN, ...)

  # Reassembling the levels, but now formatted
  bin_levels_new <- paste0(ifelse(bin_lower_inclusive, "[", "("),
                           bin_value_min, " - ", bin_value_max,
                           ifelse(bin_upper_inclusive, "]", ")"))
  levels(intervals) <- bin_levels_new

  return(intervals)
}


