#' Find EPSG code for State Plane CRS
#'
#' `zr_get_crs()` uses state plane geometry to find an appropriate NAD83
#' crs for the input geometry
#'
#' @details
#' The state plane data was compiled using [ArcGIS Hub](https://hub.arcgis.com/datasets/esri::usa-state-plane-zones-nad83/explore)
#' and the [epsg.io](https://epsg.io/) website.
#'
#' @param geom_data Either a simple feature collection or a
#' file path to a `.geojson` file.
#' @param large_area Set this to `TRUE` if your data may
#' cross multiple state planes. It will be a bit longer but
#' find the state plane that covers it the best
#'
#' @returns The appropriate epsg code as an integer
#' @export
#'
#' @examples
zr_get_crs <- function(geom_data, large_area = FALSE){

  # find out what type of data was input
  if (inherits(geom_data, "sf")){ # it is alread an sf object
    geom <- geom_data
  } else if (inherits(geom_data, "character")){ # it might be a file path
    if (file.exists(geom_data)){ # it is a file path
      geom <- sf::st_read(geom_data, quiet = TRUE)
    } else{
      stop("Input must be existing file or sf object")
    }
  } else{
    stop("Input must be existing file or sf object")
  }

  if (large_area == FALSE){
    geom <- geom[1,]
  }

  intersections <- sf::st_intersects(sf::st_make_valid(geom), state_planes_crs)

  # list each intersecting idx and
  # count how many times it was intersected
  tbl <- table(unlist(intersections))

  # get the idx that was intersected the most
  sp_idx <- as.numeric(names(tbl)[which.max(tbl)])

  # use sp_idx, to get the correct crs code
  crs <- state_planes_crs[[sp_idx, "EPSG_NAD83"]]

  return(crs)
}
