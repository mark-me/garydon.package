# Copy a dataframe to the clipboard.
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

