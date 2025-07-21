#' Get path to zoneR's example files
#'
#' zoneR has a few example OZFS files stored internally. `zr_example_files`
#' returns either a list of names of the example files, or the file path
#' of a specified file.
#'
#' @param file The name of the example file. If NULL, then a list
#' of names of example files will be returned
#'
#' @returns Either a list of names of the example files, or a path
#' to the specified file
#' @export
#'
#' @examples
#' zr_example_files()
#' zr_example_files("Paradise.parcel)
#'
zr_example_files <- function(file = NULL){
  if (is.null(file)) {
    dir(system.file("extdata", package = "zoneR"))[dir(system.file("extdata", package = "zoneR")) != "sp_crs.geojson"]
  } else {
    system.file("extdata", file, package = "zoneR", mustWork = TRUE)
  }
}
