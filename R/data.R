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
#'   \item{geometry}{Zone Geometry}
#'
#' }
#' @source <https://hub.arcgis.com/datasets/esri::usa-state-plane-zones-nad83/explore>
#' @source <https://epsg.io/>
"state_planes_crs"
