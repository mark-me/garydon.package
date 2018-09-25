#' Generating a new 'anonymized' version of a dataframe
#'
#' The data frame will be anonymized by doing 3 things:
#' \enumerate{
#'   \item Replicating about \emph{italics}perc_dev of the rows randomly.
#'   \item Getting a random subset from those of about \emph{italics}1 - perc_dev
#'   \item Randomizing numeric values by multiplying them by random values between 1 - perc_dev and 1 + perc_dev
#'   \item Randomizing non-numeric values by reassigning the randomly to new rows.
#' }
#' @param tbl_in The data frame containing the data to be anonymized
#' @param vec_col_names vector of column names that need to be anonymized
#' @param lat The name of the column containing the latitude data
#' @keywords anonymization demo
#' @export
#' @examples
#' tbl_companies_uk_anon <- anonymize_tbl(tbl_companies_uk,
#'                                        vec_col_names = c("LONGITUDE_RA",
#'                                                          "LATITUDE_RA",
#'                                                          "ESTABLISHMENT_DATE")
anonymize_tbl <- function(tbl_in, vec_col_names, perc_dev = 0.2) {

  set.seed(42)
  # Creating more rows by repeating random 20% of the rows
  qty_random_extra <- round(nrow(tbl_in) * 1 + perc_dev, 0)
  tbl_in_random <- tbl_in[sample(qty_random_extra), ] # Drawing random 20% of original data
  tbl_sample <- rbind(tbl_in, tbl_in_random)          # Adding random sample to original data
  rm(qty_random_extra, tbl_in_random)

  # Selecting a random subset of those
  qty_sample_lower <- round(nrow(tbl_in) * 1 - perc_dev, 0)                   # Lower limit of random rows picked
  qty_sample_upper <- round(nrow(tbl_in) * 1 + perc_dev, 0)                   # Upper limit of random rows picked
  qty_random_sample <- round(runif(1, qty_sample_lower, qty_sample_upper), 0) # Number of new randomized sample rows
  tbl_sample <- tbl_in[sample(qty_random_sample), ]                           # Drawing of sample
  rm(qty_sample_lower, qty_sample_upper, qty_random_sample)

  # Randomizing column values
  for(col_name in vec_col_names){

    column <- tbl_sample[ , col_name]

    if("numeric" %in% class(column)) {
      # Make random variations
      tbl_sample[ , col_name] <- randomize_num_column(column, perc_dev)

    } else {
      # Shuffle row values
      tbl_sample[ , col_name] <- column[sample(1:length(column))]
    }

  }

  return(tbl_sample)
}

# Anonymize the values of numeric columns ----
randomize_num_column <- function(column, perc_dev) {
  #' Anonymize the values of numeric columns by multiplying their current values
  set.seed(42)

  random <- runif(length(column), 1 - perc_dev, 1 + perc_dev)
  column <- column * random

  return(column)
}
