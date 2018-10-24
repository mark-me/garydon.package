#' Make shape file of data frame from long and lat columns
#'
#' @param df The data frame containing the data to be mapped
#' @param lon The name of the column containing the longitude data
#' @param lat The name of the column containing the latitude data
#' @keywords maps
#' @export
#' @examples
#' sp_companies_uk <- create_spatial_df(tbl_companies_uk, lon = "LONGITUDE_RA", lat = "LATITUDE_RA")
create_spatial_df <- function(df, lon = "lon", lat = "lat") {

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

#' Formula to calculate distances between two lon-lat points
#'
#' @param lon1 The name of the column containing the first point longitude
#' @param lat1 The name of the column containing the first point latitude
#' @param lon2 The name of the column containing the second point longitude
#' @param lat2 The name of the column containing the second point latitude
#' @keywords maps distance longitude latitude
#' @export
#' @examples
#' kilometers <- distance_degrees_to_km(lon1, lat1, lon2, lat2)
distance_degrees_to_km <- function (lon1, lat1, lon2, lat2)
{
  earth_radius <- 6378.145
  radian <- pi / 180
  lat1_rad <- lat1 * radian
  lon1_rad <- lon1 * radian
  lat2_rad <- lat2 * radian
  lon2_rad <- lon2 * radian
  dist_lon <- lon2_rad - lon1_rad
  dist_lat <- lat2_rad - lat1_rad
  a <- (sin(dist_lat / 2)) ^ 2 + cos(lat1_rad) * cos(lat2_rad) * (sin(dist_lon / 2)) ^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))

  dist_km <- earth_radius * c
  return(dist_km)
}
