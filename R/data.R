#' State Planes with Projected CRS Data
#'
#' This data set contains the geometry for each US State Plane Zone
#' along with the EPSG code for its projected coordinate system.
#' The geometry was gathered from an arcgis hub data set, and
#' the epsg codes were gathered from epsg.io
#'
#' @format ## `state_planes_crs`
#' Simple feature collection with 121 features and 4 fields
#' \describe{
#'   \item{OBJECTID}{Row Index}
#'   \item{ZONE}{Zone Abbreviation}
#'   \item{ZONENAME}{Zone Name}
#'   \item{EPSG_NAD83}{Projected CRS EPSG Code}
#'
#' }
#' @source <https://hub.arcgis.com/datasets/esri::usa-state-plane-zones-nad83/explore>
#' @source <https://epsg.io/>
"state_planes_crs"

#' Possible Constraints to Check
#'
#' This character vector lists all the possible constraints that can be
#' checked in the `zr_run_zoning_checks()` functions
#'
#' @format
#' A character vector
#' c("res_type",
#' "far",
#' "fl_area",
#' "fl_area_first",
#' "fl_area_top",
#' "footprint",
#' "height",
#' "height_eave",
#' "lot_cov_bldg",
#' "lot_area",
#' "parking_enclosed",
#' "stories",
#' "unit_0bed",
#' "unit_1bed",
#' "unit_2bed",
#' "unit_3bed",
#' "unit_4bed",
#' "unit_density",
#' "unit_pct_0bed",
#' "unit_pct_1bed",
#' "unit_pct_2bed",
#' "unit_pct_3bed",
#' "unit_pct_4bed",
#' "total_units",
#' "unit_size_avg",
#' "unit_size",
#' "bldg_fit",
#' "overlay")
#
"possible_checks"
