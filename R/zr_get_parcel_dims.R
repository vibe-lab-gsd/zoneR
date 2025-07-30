#' Isolate dimensional rows from OZFS parcel data
#'
#' `zr_get_parcel_dims()` takes the data in the `.parcel` file and creates a
#' simple features data frame with only the geometry representing the parcel centroids.
#' The data frame contains the dimensional information related to each parcel.
#'
#' @section Parcel File:
#' According to OZFS, the `.parcel` file follows geojson format with geometry for
#' each feature. The features with parcel side geometry store information on the
#' parcel id and the side label. The features with parcel centroid geometry store
#' parcel id information along with the parcel's width, depth, and area.
#' `zr_get_parcel_geo()` filters this data into a data frame that just has side
#' geometries and labels. `zr_get_parcel_dims()` filters this data into a data frame
#' that only has centroid geometries and the dimensional information.
#'
#' @param parcels_file Either a file path to an OZFS `.parcel` file or a
#' simple features object with the same data.
#'
#' @returns Returns a simple features object with only dimensional and centroid
#' data for each parcel. It also adds a lot_type column specifying whether or
#' not it is a corner parcel.
#' @export
#'
#' @examples
#' file <- zr_example_files("Paradise.parcel")
#'
#' parcel_dims <- zr_get_parcel_dims(file)
#' head(parcel_dims)
zr_get_parcel_dims <- function(parcels_file){
  if (inherits(parcels_file, "character")){ # then it is a file path
    parcels_sf <- tryCatch({
      sf::st_read(parcels_file, quiet = TRUE)
    }, error = function(e) {
      stop("Unable to open file. Check to make sure it is a proper geojson")
    })
  } else if (inherits(parcels_file, "sf")){ # then it is an sf object
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
