#' Create data frame with unit info
#'
#' `zr_get_unit_info()` takes a json representing a building and
#' returns a data frame with data for each unit in the building.
#'
#' @inheritParams zr_get_variables
#'
#' @returns a data frame with information for each unit in the building
#' @export
#'
#' @examples
zr_get_unit_info <- function(bldg_data){

  if (class(bldg_data)[[1]] == "character"){
    listed_json <- tryCatch({
      rjson::fromJSON(file = bldg_data)
    }, error = function(e) {
      stop("bldg_data must be a file path to an OZFS *.bldg file or a list created from said file")
    })
  } else if (class(zoning_data)[[1]] == "list"){
    listed_json <- bldg_data
  } else{
    stop("Improper input: bldg_data")
  }

  if (is.null(listed_json$bldg_info) | is.null(listed_json$unit_info) | is.null(listed_json$level_info)){
    stop("Improper format: json must contain bldg_info, unit_info, and level_info sections")
  }

  fl_area_val <- c()
  bedrooms_val <- c()
  qty_val <- c()
  for (unit in listed_json$unit_info){
    fl_area_val <- c(fl_area_val, unit$fl_area)
    bedrooms_val <- c(bedrooms_val, unit$bedrooms)
    qty_val <- c(qty_val, unit$qty)
  }
  unit_info_df <- data.frame(fl_area = fl_area_val,
                             bedrooms = bedrooms_val,
                             qty = qty_val)

  return(unit_info_df)

}


