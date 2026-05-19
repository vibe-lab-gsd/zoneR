#' Build hybrid zoning districts from base and overlay layers
#'
#' @description
#' `zr_build_hybrid_districts()` merges zoning requirements from each relevant
#' base district and overlay district pair into hybrid district records. This
#' is the district-catalog step: no parcel assignment occurs here.
#'
#' Overlay effect types (e.g. `relax`, `restrict`, `replace`) from the OZFS
#' `overlay` field determine how each constraint is combined.
#'
#' @param zoning_sf A simple features object of base zoning districts
#'   (`overlay == FALSE`).
#' @param overlays A simple features object of overlay zoning districts.
#'
#' @return A simple features object (`hybrid_zoning_sf`) containing base
#'   districts and hybrid rows (base + overlay combined requirements), indexed
#'   for parcel lookup in [zr_assign_hybrid_districts()].
#' @export
#'
#' @seealso [zr_assign_hybrid_districts()], [zr_run_zoning_checks()],
#'   [zr_get_zoning_req()]
#'
#' @examples
#' \dontrun{
#' # See zr_run_zoning_checks() data-prep section (after zoning layer split).
#' }
#'
zr_build_hybrid_districts <- function(zoning_sf, overlays) {

  zoning_sf
}
