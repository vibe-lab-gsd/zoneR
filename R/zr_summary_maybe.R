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
zr_summary_maybe <- function(detailed_result){

  summary <- detailed_result$reason[detailed_result$allowed == "MAYBE"] |>
    strsplit(", ") |>
    unlist() |>
    table() |>
    as.data.frame() |>
    arrange(-Freq) |>
    mutate(`Reason for ambiguity` =
             reasons$maybe_explain[match(Var1, reasons$short_reason)]) |>
    rename(`Number of parcels` = Freq) |>
    select(`Reason for ambiguity`, `Number of parcels`)

  return(summary)
}
