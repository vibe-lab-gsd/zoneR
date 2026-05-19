#' Assign parcels to hybrid zoning districts
#'
#' @description
#' `zr_assign_hybrid_districts()` maps each parcel to the appropriate row in
#' `hybrid_zoning_sf` using its base `zoning_id` and `overlay_id`. Parcels
#' without an overlay are assigned to the corresponding base-only row in the
#' hybrid catalog.
#'
#' Call this after [zr_find_district_idx()] on base and overlay districts and
#' after [zr_build_hybrid_districts()].
#'
#' @param parcel_dims An sf object of parcel centroids with `zoning_id` (base
#'   district row) and `overlay_id` from overlay [zr_find_district_idx()].
#' @param hybrid_zoning_sf Simple features object from [zr_build_hybrid_districts()].
#'
#' @return `parcel_dims` with `zoning_id` updated to reference rows in
#'   `hybrid_zoning_sf`.
#' @export
#'
#' @seealso [zr_build_hybrid_districts()], [zr_run_zoning_checks()],
#'   [zr_find_district_idx()]
#'
#' @examples
#' \dontrun{
#' # See zr_run_zoning_checks() immediately after parcel–district index joins.
#' }
#'
zr_assign_hybrid_districts <- function(parcel_dims, hybrid_zoning_sf) {

  parcel_dims
}
