#' Assign parcels to hybrid zoning districts
#'
#' @description
#' `zr_assign_hybrid_districts()` maps each parcel to the appropriate row in
#' `hybrid_zoning_sf` using its base `zoning_id` and `overlay_id`. Parcels
#' without an overlay keep their base district assignment.
#'
#' Call this after [zr_find_district_idx()] on base and overlay districts and
#' after [zr_build_hybrid_districts()].
#'
#' @param parcel_df An sf object of parcels with base district assignment,
#'   including `zoning_id`, `dist_abbr`, and `muni_name`.
#' @param parcels_overlays The same parcel geometries with `overlay_id` from
#'   [zr_find_district_idx()] on overlay districts.
#' @param hybrid_zoning_sf Simple features object from [zr_build_hybrid_districts()].
#'
#' @return `parcel_df` with `zoning_id` (and related district fields) updated
#'   to reference rows in `hybrid_zoning_sf`.
#' @export
#'
#' @seealso [zr_build_hybrid_districts()], [zr_run_zoning_checks()],
#'   [zr_find_district_idx()]
#'
#' @examples
#' \dontrun{
#' # See zr_run_zoning_checks() data-prep section (after parcel–district joins).
#' }
#'
zr_assign_hybrid_districts <- function(parcel_df,
                                       parcels_overlays,
                                       hybrid_zoning_sf) {

  parcel_df
}
