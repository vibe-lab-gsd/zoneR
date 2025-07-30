#' Isolate side geometry from OZFS parcel data frame
#'
#' `zr_get_parcel_geo()` takes the data in the `.parcel` file and creates a
#' simple features data frame with only the geometry representing the parcel sides.
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
#'
#' @param parcels_file Either a file path to an OZFS `.parcel` file
#' or a simple features object with the same data.
#'
#' @returns Returns a simple features object with only the side geometries
#' of the parcels.
#' @export
#'
#' @examples
#' file <- zr_example_files("Paradise.parcel")
#'
#' parcel_geo <- zr_get_parcel_geo(file)
#'
#' # parcels with unknown side lables
#' head(parcel_geo)
#'
#' # parcels with labeled sides
#' tail(parcel_geo)
zr_get_parcel_geo <- function(parcels_file){
  if (inherits(parcels_file, "character")){ # then it is a file path
    parcels_sf <- tryCatch({
      sf::st_read(parcels_file, quiet = TRUE)
    }, error = function(e) {
      stop("Unable to open file. Is it the proper geojson format? Does the file exist?")
    })
  } else if (inherits(parcels_file, "sf")){ # then it is an sf object
    parcels_sf <- parcels_file
  } else{ # it isn't going to work
    stop("improper input")
  }

  # filter to just have the side geometry if they are labeled
  parcels_geo <- parcels_sf |>
    dplyr::filter(side != "centroid") |>
    dplyr::select(parcel_id, side)

  return(parcels_geo)
}

