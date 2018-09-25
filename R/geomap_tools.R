#' Make shape file of data frame from long and lat columns
#'
#' @param df The data frame containing the data to be mapped
#' @param lon The name of the column containing the longitude data
#' @param lat The name of the column containing the latitude data
#' @keywords maps
#' @export
#' @examples
#' sp_companies_uk <- create_spatial_df(tbl_companies_uk, lon = "LONGITUDE_RA", lat = "LATITUDE_RA")
create_spatial_df <- function(df, lon = "LONGITUDE_RA", lat = "lat") {

  sp_new <- data.frame(df)
  sp::coordinates(sp_new) <- c(lon, lat)   # Convert data frame to spatial object
  sp::proj4string(sp_new) <- sp::CRS("+proj=longlat +ellps=clrk66") # Setting up a initial projection

  return(sp_new)
}

#' Find key for merging Spatial* objects
#'
#' @param df The data frame containing the data to be mapped
#' @param lon The name of the column containing the longitude data
#' @param lat The name of the column containing the latitude data
#' @keywords maps
#' @export
#' @examples
#' tbl_merge <- match_sp(sp_companies_uk, sp_uk_lieutenancy)
match_sp <- function(sp_from, sp_to) {

  # Set both spatial objects to same projection
  sp_from <- sp::spTransform(sp_from, sp_to@proj4string)
  # Create an identifier for combining data
  sp_to@data$id_merge <- rownames(sp_to@data)
  # Find the overlay between the two spatial objects
  df_merge <- sp::over(sp_from, sp_to)
  # Add the overlay two the original data
  df_merge <- cbind(id_merge = df_merge$id_merge,
                    sp_from@data)
  return(df_merge)
}

