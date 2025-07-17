#' Is the building allowed base on use_type?
#'
#' Campares the building use type with the permitted use types in the zoning
#' code and returns TRUE or FALSE
#'
#' @param vars the result from the `zr_get_variables()` function.
#' If this data frame is supplied, bldg_data, parcel_data, and zoning_data
#' are not needed.
#' @param district_data one row (representing one district) of a
#' zoning data frame created from the OZFS *.zoning file
#'
#' @returns TRUE or FALSE
#' @export
#'
#' @examples
zr_check_res_type <- function(vars, district_data){
  check <- vars$res_type %in% district_data$res_types_allowed[[1]]
  return(check)
}

