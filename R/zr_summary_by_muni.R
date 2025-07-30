#' Summarize the results of zr_run_zoning_checks() by municipality
#'
#' `zr_summary_by_muni()` summarizes results of zr_run_zoning_checks() by municipality
#'
#'
#' @param detailed_result The output simple features data frame from
#' zr_run_zoning_checks()
#'
#' @returns A tibble summarizing the number of parcels with TRUE, MAYBE, and FALSE
#' results by municipality
#' @export
#'
#' @examples
zr_summary_by_muni <- function(detailed_result){

  totals <- detailed_result |>
    sf::st_drop_geometry() |>
    dplyr::summarise(num_allowed = sum(allowed == "TRUE"),
                     num_maybe = sum(allowed == "MAYBE"),
                     num_prohib = sum(allowed == "FALSE")) |>
    dplyr::mutate(muni_name = "Total") |>
    dplyr::select(muni_name, num_allowed, num_maybe, num_prohib)

  summary |>
    sf::st_drop_geometry() |>
    dplyr::group_by(muni_name) |>
    dplyr::summarise(num_allowed = sum(allowed == "TRUE"),
                     num_maybe = sum(allowed == "MAYBE"),
                     num_prohib = sum(allowed == "FALSE")) |>
    dplyr::arrange(-num_prohib) |>
    rbind(totals)

  return(summary)
}
