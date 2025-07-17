#' Isolate dimensional rows from tidyparcel data
#'
#' @param parcels_file Either a file path to the a parcels geojson file or an sf object with the same data.
#'
#' @returns an sf object with only dimensional and centroid data for each parcel
#' @export
#'
#' @examples
zr_get_parcel_dims <- function(parcels_file){
  if (class(parcels_file)[[1]] == "character"){ # then it is a file path
    parcels_sf <- tryCatch({
      sf::st_read(parcels_file, quiet = TRUE)
    }, error = function(e) {
      stop("Unable to open file. Check to make sure it is a proper geojson")
    })
  } else if (class(parcels_file)[[1]] == "sf"){ # then it is an sf object
    parcels_sf <- parcels_file
  } else{ # it isn't going to work
    stop("improper input")
  }

  corner_parcels <- parcels_sf |>
    dplyr::filter(side == "exterior side")

  # filter to just the centroids so we only have dimensions and one parcel per row
  parcels_dim <- parcels_sf |>
    dplyr::filter(side == "centroid") |>
    dplyr::mutate(lot_type = ifelse(parcel_id %in% corner_parcels$parcel_id, "corner", "regular")) |>
    dplyr::select(parcel_id, lot_width, lot_depth, lot_area, lot_type)



  corner_parcels

  return(parcels_dim)
}
