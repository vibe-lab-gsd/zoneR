#' Create a parcel's buildable area
#'
#' `zr_get_buildable_area()` takes a parcel with setback information and
#' produces a polygon representing the buildable area of the parcel.
#'
#' @param parcel_with_setbacks A parcel_geo object that has setback
#' information added. parcel_with_setbacks is the output of the
#' [zr_add_setbacks()] function.
#' @param crs The projected Coordinate Reference System for the study area.
#' Must be in meters. If NULL, crs is calculated using `zr_get_crs()`.
#'
#' @return
#' Returns a polygon representing the buildable area of the parcel.
#' @export
#'
#' @example inst/examples/fit_example.R
zr_get_buildable_area <- function(parcel_with_setbacks, crs = NULL){

  if (is.null(crs)){
    crs <- zr_get_crs(parcel_with_setbacks)
  }

  parcel_with_setbacks <- sf::st_transform(parcel_with_setbacks, crs)

  # make tidyparcel a polygon
  polygon <- parcel_with_setbacks |>
    sf::st_union() |>
    sf::st_polygonize() |>
    sf::st_collection_extract()

  if (nrow(parcel_with_setbacks[!is.na(parcel_with_setbacks$setback),]) == 0){
    return(list(polygon))
  }

  # seprate the min and max setbacks
  parcel_with_setbacks$min_setback <- unlist(lapply(parcel_with_setbacks$setback, min))
  parcel_with_setbacks$max_setback <- unlist(lapply(parcel_with_setbacks$setback, max))

  if (sum(parcel_with_setbacks$min_setback, na.rm = TRUE) == 0 & sum(parcel_with_setbacks$max_setback, na.rm = TRUE) == 0){
    return(list(polygon))
  }

  parcel_with_setbacks <- parcel_with_setbacks  |>
    dplyr::mutate(min_setback = ifelse(is.na(min_setback), 0.1, min_setback),
           max_setback = ifelse(is.na(max_setback), 0.1, max_setback))

  # convert feet to meters
  parcel_with_setbacks <- parcel_with_setbacks |>
    dplyr::mutate(min_setback = min_setback * 0.3048,
           max_setback = max_setback * 0.3048)

  if (identical(parcel_with_setbacks$min_setback,parcel_with_setbacks$max_setback)){ # just one setback for each side

    # put a buffer on each side (need to convert to meters)
    buffered_sides <- parcel_with_setbacks |>
      dplyr::mutate(geometry = sf::st_buffer(geometry,min_setback))

    # make the buffered sides all one polygon
    buffered_polygon <- sf::st_union(buffered_sides)
    buildable_area <- sf::st_difference(sf::st_make_valid(polygon),sf::st_make_valid(buffered_polygon))

    # if there happen to be more than one polygon created, then we
    # assume that the polygon with the most points is the one that
    # represents the buildable area the best
    if (length(buildable_area) > 1){
      longer_geometry_idx <- sapply(buildable_area, length) |> which.max()
      buildable_area <- buildable_area[longer_geometry_idx]
    }

    buildable_area <- list(buildable_area)


  } else{ #multiple setback possibilities

    # put a buffer on each side (need to convert to meters)
    buffered_sides_relaxed <- parcel_with_setbacks |>
      dplyr::mutate(geometry = sf::st_buffer(geometry,min_setback))

    # make the buffered sides all one polygon
    buffered_polygon_relaxed <- sf::st_union(buffered_sides_relaxed)
    buildable_area_relaxed <- sf::st_difference(sf::st_make_valid(polygon),sf::st_make_valid(buffered_polygon_relaxed))

    # if there happen to be more than one polygon created, then we
    # assume that the polygon with the most points is the one that
    # represents the buildable area the best
    if (length(buildable_area_relaxed) > 1){
      longer_geometry_idx <- sapply(buildable_area_relaxed, length) |> which.max()
      buildable_area_relaxed <- buildable_area_relaxed[longer_geometry_idx]
    }

    # put a buffer on each side (need to convert to meters)
    buffered_sides_strict <- parcel_with_setbacks |>
      dplyr::mutate(geometry = sf::st_buffer(geometry,max_setback))

    # make the buffered sides all one polygon
    buffered_polygon_strict <- sf::st_union(buffered_sides_strict)
    buildable_area_strict <- sf::st_difference(sf::st_make_valid(polygon),sf::st_make_valid(buffered_polygon_strict))

    # if there happen to be more than one polygon created, then we
    # assume that the polygon with the most points is the one that
    # represents the buildable area the best
    if (length(buildable_area_strict) > 1){
      longer_geometry_idx <- sapply(buildable_area_strict, length) |> which.max()
      buildable_area_strict <- buildable_area_strict[longer_geometry_idx]
    }

    buildable_area <- list(buildable_area_strict, buildable_area_relaxed)

  }

  return(buildable_area)

}
