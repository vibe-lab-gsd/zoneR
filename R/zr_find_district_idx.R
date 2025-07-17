#' Find corresponding zoning district
#'
#' @description
#' `zr_find_district_idx()` uses [sf::st_join()] to adds a new column to the parcels data that
#' contains the tidyzoning row index of the district the parcel is in
#'
#' @param parcels_centroids_sf An sf object with the parcel centroids
#' @param tidyzoning A simple features object with a row for each zoning district and columns holding the geojson formatted zoning requirements of each district.
#' @param idx_col_name The name of the new column that will be added with the district index
#'
#' @return The same parcels centroids object that was input but with an additional
#' column stating the row number of the corresponding tidyzoning district
#' @export
#'
zr_find_district_idx <- function(parcels_centroids_sf, tidyzoning, idx_col_name = "zoning_id"){

  tidyzoning <- sf::st_make_valid(tidyzoning)
  tidyzoning[[idx_col_name]] <- 1:nrow(tidyzoning)

  parcels_with_zoning_id <- sf::st_join(sf::st_make_valid(parcels_centroids_sf), tidyzoning[idx_col_name])

  return(parcels_with_zoning_id)

}
