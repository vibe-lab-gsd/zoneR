#' Summarize the reasons for maybe results
#'
#' `zr_summary_by_muni()` summarizes results reasons given for maybe results from
#' the output of zr_run_zoning_checks()
#'
#' @param detailed_result The output simple features data frame from
#' zr_run_zoning_checks()
#' @param muni The city to summarize data for. Defaults to "All" to aggregate
#' across all cities include in the results file.
#'
#' @returns A tibble summarizing the reasons for maybe values.
#'
#' @export
#'
#' @example inst/examples/summary_examples.R
zr_summary_maybe <- function(detailed_result,
                             muni = "All"){

  if (!"MABYE" %in% detailed_result$allowed){
    return(cat("No MAYBE values found in results\n"))
  }

  if(muni != "All") {
    detailed_result <- detailed_result |>
      dplyr::filter(muni_name == muni)
  }

  summary <- detailed_result$reason[detailed_result$allowed == "MAYBE"] |>
    strsplit(", ") |>
    unlist() |>
    table() |>
    as.data.frame() |>
    dplyr::arrange(-Freq) |>
    dplyr::mutate(`Reason for ambiguity` =
             reasons$maybe_explain[match(Var1, reasons$short_reason)]) |>
    dplyr::rename(`Number of parcels` = Freq) |>
    dplyr::select(`Reason for ambiguity`, `Number of parcels`)

  return(summary)
}
