#' Sets first letter of a string to capital
#'
#' @param x The string you want to apply the formatting to
#' @return A formatted string
#' @export
#' @examples
#' str_firstup("ThIs iS One uGLy sTRIng!")
str_firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
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
  if (is.na(scale) | scale == "normal") {
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
#' format_currency(amount = 1000000, currency = "EUR", scale = "M")
format_currency <- function(amount, currency = c("EUR", "GBP"), number_decimals = 2, scale = NA) {

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

#' Formats a number to percentage by adding . (thousands), "," (decimals) and % to the number ----
#'
#' @param percentage The number you want to format
#' @param num_digits The number of decimal places you want shown. Default is 1
#' @return A string containing the formatted number
#' @export
#' @examples
#' format_percent(percentage = 0.123, format_EN = TRUE)
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
    idx_end <- qty_total - idx_start
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

  idx_start <- qty_total - qty_characters

  return(substr(text, idx_start, qty_total))
}

