#' Is the building allowed base on zoning constraints?
#'
#' `zr_check_constraints()` compares the building characteristics
#' against many of the main constraints in the `.zoning` file. It returns
#' a one-row data frame with a column for each constraint it checked against.
#'
#' @param vars the result from the `zr_get_variables()` function.
#' If this data frame is supplied, bldg_data, parcel_data, and zoning_data
#' are not needed.
#' @param zoning_req The output of `zr_get_zoning_req()`. A data frame with
#' constraint values
#' @param checks A list of all the checks that should take place. The default is
#' every check possible. Note, if a zoning file doesn't have zoning info for one
#' of the constraints listed in the checks variable, then it is assumed that
#' building characteristic is allowed.
#'
#' @returns
#' @export
#'
#' @examples
#' # preparing  all the OZFS files to run functions
#' zoning_file <- zr_example_files("Paradise.zoning")
#' parcel_file <- zr_example_files("Paradise.parcel")
#' bldg_file <- zr_example_files("2_fam.bldg")
#'
#' # getting zoning file as simple features object and as a list
#' zoning_sf <- sf::st_read(zoning_file, quiet = TRUE)
#' zoning_data <- rjson::fromJSON(file = zoning_file)
#' # getting parcel file as simple features object
#' parcels_sf <- sf::st_read(parcel_file, quiet = TRUE)
#' # getting bldg file as a list
#' bldg_data <- rjson::fromJSON(file = bldg_file)
#'
#' # get parcel_dims
#' parcel_dims <- zr_get_parcel_dims(parcels_sf)
#'
#' # use parcel_dims to create parcel_df with a zoning_id column
#' parcel_df <- zr_find_district_idx(parcel_dims, zoning_sf)
#'
#' # choose just one parcel for the example
#' parcel_data <- parcel_df[parcel_df$parcel_id == "Wise_County_combined_parcel_10300",]
#' # get the row of the district that parcel is apart of
#' district_data <- zoning_sf[parcel_data$zoning_id,]
#'
#' # get variables
#' vars <- zr_get_variables(bldg_data, parcel_data, district_data, zoning_data)
#'
#' # get zoning requirements
#' zoning_req <- zr_get_zoning_req(district_data = district_data, vars = vars)
#'
#' constraints_check <- zr_check_constraints(vars, zoning_req)
#'
zr_check_constraints <- function(vars,
                                 zoning_req,
                                 checks = possible_checks[!possible_checks %in% c("res_type","unit_size","bldg_fit","overlay")]){

  # if the zonning_req is "character" and not "data.frame", there were no zoning requirements recorded.
  if (inherits(zoning_req,"character")){
    return("No zoning requirements recorded for this district")
  }

  filtered_req <- zoning_req[zoning_req$constraint_name %in% checks,]


  for (i in 1:nrow(filtered_req)){
    # establish the constraint we are looking at
    constraint <- filtered_req$constraint_name[[i]]
    constraint_df <-filtered_req[i,]

    value <- vars[[constraint]]

    if (is.null(value)){
      filtered_req[i,"warning"] <- "Variable not found"
      filtered_req[i,"allowed"] <- TRUE
      next
    }

    min_req <- ifelse(is.na(constraint_df$min_value),0,constraint_df$min_value)
    max_req <- ifelse(is.na(constraint_df$max_value),100000,constraint_df$max_value)

    min_requirement <- min_req[[1]]
    max_requirement <- max_req[[1]]
    # this tests for the usual case when each min and max requirements have one value
    if (length(min_requirement) & length(max_requirement) == 1){
      filtered_req[i,"allowed"] <- value >= min_requirement & value <= max_requirement
      next
    }

    # this is where multiple values are listed for a unique zoning requirement
    # assign 2 minimum values
    min_check_1 <- min(min_requirement) <= value
    min_check_2 <- max(min_requirement) <= value
    if (is.null(zoning_req[zoning_req$constraint_name == constraint, "min_val_note"][[1]])){
      min_val_either <- FALSE
    } else if(is.na(zoning_req[zoning_req$constraint_name == constraint, "min_val_note"][[1]])){
      min_val_either <- FALSE
    } else{
      min_val_either <- zoning_req[zoning_req$constraint_name == constraint, "min_val_note"][[1]] == "either"
    }

    # see if the minimum values are met
    if (min_val_either == TRUE){
      if (min_check_1 == FALSE & min_check_2 == FALSE){
        min_check <- FALSE
      } else{
        min_check <- TRUE
      }

    } else{
      if (min_check_1 == TRUE & min_check_2 == TRUE){
        min_check <- TRUE
      } else if (min_check_1 == FALSE & min_check_2 == FALSE){
        min_check <- FALSE
      } else{
        min_check <- "MAYBE"
      }
    }

    # assign 2 maximum values
    max_check_1 <- min(max_requirement) >= value
    max_check_2 <- max(max_requirement) >= value
    if (is.null(zoning_req[zoning_req$constraint_name == constraint, "max_val_note"][[1]])){
      max_val_either <- FALSE
    } else if(is.na(zoning_req[zoning_req$constraint_name == constraint, "max_val_note"][[1]])){
      max_val_either <- FALSE
    } else{
      max_val_either <- zoning_req[zoning_req$constraint_name == constraint, "max_val_note"][[1]] == "either"
    }

    # see if the maximum values are met
    if (max_val_either == TRUE){
      if (max_check_1 == FALSE & max_check_2 == FALSE){
        max_check <- FALSE
      } else{
        max_check <- TRUE
      }

    } else{
      if (max_check_1 == TRUE & max_check_2 == TRUE){
        max_check <- TRUE
      } else if (max_check_1 == FALSE & max_check_2 == FALSE){
        max_check <- FALSE
      } else{
        max_check <- "MAYBE"
      }
    }

    # use both min and max value check to see if it is allowed
    if (max_check == FALSE | min_check == FALSE){
      filtered_req[i,"allowed"] <- FALSE
      next
    } else if (max_check == TRUE & min_check == TRUE){
      filtered_req[i,"allowed"] <- TRUE
      next
    } else{
      explanation <- c()
      if (!is.na(zoning_req[zoning_req$constraint_name == constraint, "min_val_error"][[1]])){
        explanation <- c(explanation,zoning_req[zoning_req$constraint_name == constraint,"min_val_error"][[1]])
      }
      if (!is.na(zoning_req[zoning_req$constraint_name == constraint, "max_val_error"][[1]])){
        explanation <- c(explanation,zoning_req[zoning_req$constraint_name == constraint,"max_val_error"][[1]])
      }

      if (length(explanation) > 0){
        filtered_req[i,"warning"] <- explanation
      }
      filtered_req[i,"allowed"] <- "MAYBE"
      next
    }

  }

  if (is.null(filtered_req$warning)){
    checked_df <- filtered_req |>
      dplyr::select(constraint_name, allowed)
  } else{
    checked_df <- filtered_req |>
      dplyr::select(constraint_name, allowed, warning)
  }

  return(checked_df)

}
