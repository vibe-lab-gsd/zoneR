#' Isolate known side geometry from tidyparcel data
#'
#' @param parcels_file Either a file path to the a parcels geojson file or an sf object with the same data.
#'
#' @returns an sf object with only the side geometries of the parcels with identified sides
#' @export
#'
#' @examples
zr_get_parcel_geo <- function(parcels_file){
  if (class(parcels_file)[[1]] == "character"){ # then it is a file path
    parcels_sf <- tryCatch({
      sf::st_read(parcels_file, quiet = TRUE)
    }, error = function(e) {
      stop("Unable to open file. Is it the proper geojson format? Does the file exist?")
    })
  } else if (class(parcels_file)[[1]] == "sf"){ # then it is an sf object
    parcels_sf <- parcels_file
  } else{ # it isn't going to work
    stop("improper input")
  }

  # filter to just have the side geometry if they are labeled
  parcels_geo <- parcels_sf |>
    dplyr::filter(side != "centroid") |>
    dplyr::filter(side != "unknown") |>
    dplyr::select(parcel_id, side)

  return(parcels_geo)
}

