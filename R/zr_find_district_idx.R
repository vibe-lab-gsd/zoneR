#' Find corresponding zoning district
#'
#' @description
#' `zr_find_district_idx()` uses [sf::st_join()] to adds a new column to the parcels data that
#' contains the zoning_sf row index of the district the parcel is in
#'
#' @param parcels_centroids_sf An sf object with the parcel centroids
#' @param zoning_sf A simple features object with a row for each zoning district and columns holding the geojson formatted zoning requirements of each district.
#' @param idx_col_name The name of the new column that will be added with the district index
#'
#' @return The same parcels centroids object that was input but with an additional
#' column stating the row number of the corresponding zoning_sf district
#' @export
#'
#' @examples
#' # preparing  all the OZFS files to run functions
#' zoning_file <- zr_example_files("Paradise.zoning")
#' parcel_file <- zr_example_files("Paradise.parcel")
#'
#' # getting zoning file as simple features object
#' zoning_sf <- sf::st_read(zoning_file, quiet = TRUE)
#'
#' # getting parcel file as simple features object
#' parcels_sf <- sf::st_read(parcel_file, quiet = TRUE)
#'
#' # get parcel_dims
#' parcel_dims <- zr_get_parcel_dims(parcels_sf)
#'
#' # use parcel_dims to create parcel_df with a zoning_id column
#' parcel_df <- zr_find_district_idx(parcel_dims, zoning_sf)
#'
#' # notice the zoning_id column
#' head(parcel_df)
#'
zr_find_district_idx <- function(parcels_centroids_sf, zoning_sf, idx_col_name = "zoning_id"){

  zoning_sf <- sf::st_make_valid(zoning_sf)

  if (nrow(zoning_sf) > 0){
    zoning_sf[[idx_col_name]] <- 1:nrow(zoning_sf)
  } else{
    parcels_centroids_sf[[idx_col_name]] <- NA
    return(parcels_centroids_sf)
  }
  parcels_with_zoning_id <- sf::st_join(sf::st_make_valid(parcels_centroids_sf), zoning_sf[idx_col_name])

  return(parcels_with_zoning_id)

}


