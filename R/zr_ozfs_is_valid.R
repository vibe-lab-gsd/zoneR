#' Is it formatted to OZFS standards?
#'
#' `zr_ozfs_is_valid()` TRUE or FALSE stating whether or not the files are
#' properly formatted according to OZFS standards. Use [zr_ozfs_validate] for
#' a detailed list of why files don't meet the standard.
#'
#' @param list_of_files A list of file paths to OZFS *.zoning files
#'
#' @returns TRUE or FALSE stating whether data is in valid OZFS format or not
#' @export
#'
#' @examples
zr_ozfs_is_valid <- function(list_of_files){
  result <- suppressWarnings(try(zr_ozfs_validate(list_of_files), silent = TRUE))
  if (inherits(result, "try-error")) {
    return(FALSE)
  }

  if (is.null(result)){
    return(TRUE)
  } else{
    return(FALSE)
  }
}
