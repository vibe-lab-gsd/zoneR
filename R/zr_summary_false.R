#' Summarize the reasons for maybe results
#'
#' `zr_summary_by_muni()` summarizes results reasons given for maybe results from
#' the output of zr_run_zoning_checks()
#'
#' @param detailed_result The output simple features data frame from
#' zr_run_zoning_checks()
#'
#' @returns A tibble summarizing the reasons for maybe values.
#'
#' @export
#'
#' @examples
zr_summary_false <- function(detailed_result){

  summary <- detailed_result$reason[detailed_result$allowed == "FALSE"] |>
    strsplit(", ") |>
    unlist() |>
    table() |>
    as.data.frame() |>
    dplyr::arrange(-Freq) |>
    dplyr::mutate(`Reason for prohibition` =
             reasons$false_explain[match(Var1, reasons$short_reason)]) |>
    dplyr::rename(`Number of parcels` = Freq) |>
    dplyr::select(`Reason for prohibition`, `Number of parcels`)

  return(summary)
}
