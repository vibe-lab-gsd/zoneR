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
zr_check_constraints <- function(vars,
                                 zoning_req,
                                 checks = c("far",
                                            "fl_area",
                                            "fl_area_first",
                                            "fl_area_top",
                                            "footprint",
                                            "height",
                                            "height_eave",
                                            "lot_cov_bldg",
                                            "lot_size",
                                            "parking_enclosed",
                                            "stories",
                                            "unit_0bed",
                                            "unit_1bed",
                                            "unit_2bed",
                                            "unit_3bed",
                                            "unit_4bed",
                                            "unit_density",
                                            "unit_pct_0bed",
                                            "unit_pct_1bed",
                                            "unit_pct_2bed",
                                            "unit_pct_3bed",
                                            "unit_pct_4bed",
                                            "total_units",
                                            "unit_size_avg")){

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
