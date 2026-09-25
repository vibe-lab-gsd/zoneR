#' Merge an overlay and a base district's requirements
#'
#' Takes the overlay district requirements and the base districts requirements, and
#' creates one data frame of the proper constraint values to be used in the analysis.
#'
#' @param base_reqs The data frame representing the zoning requirements for the
#' base district that was created with `zr_get_zoning_req()`
#' @param overlay_reqs The data frame representing the zoning requirements for the
#' overlay district that was created with `zr_get_zoning_req()`
#' @param overlay_type String stating whether the overlay is of type "restrict", "relax", or "replace"
#'
#' @returns One data frame that is a merger of the two input data frames according to
#' respective overly rules
#'
#' @export
#'
#' @examples
#' base_reqs <- data.frame(constraint_name = c("lot_area","setback_front"),
#'                         min_value = I(list(list(0.17), list(5))),
#'                         max_value = I(list(list(1.5), list(30))))
#' overlay_reqs <- data.frame(constraint_name = c("lot_area","setback_front"),
#'                         min_value = I(list(list(0.2), list(10))),
#'                         max_value = I(list(list(2), list(35))))
#'
#' zr_merge_overlay_reqs(base_reqs, overlay_reqs, "restrict")
#'
#' zr_merge_overlay_reqs(base_reqs, overlay_reqs, "relax")
#'
#' zr_merge_overlay_reqs(base_reqs, overlay_reqs, "replace")
#'
zr_merge_overlay_reqs <- function(base_reqs, overlay_reqs, overlay_type){

  if (is.null(base_reqs)){
    # if base_reqs is null, that means the parcel was
    # already removed in pd_check
    # we return NULL and just skip over it for the overlay check
    return(NULL)
  } else if (inherits(base_reqs, "character") & inherits(overlay_reqs, "character")){
    # if both overlay and base reqs don't have zoning requirements,
    # then just return the character stored in base reqs
    return(base_reqs)
  } else if (inherits(overlay_reqs, "character")){
    # if overlay_reqs has no zoning requirements
    # return the base_reqs
    base_reqs$min_ovly <- "base"
    base_reqs$max_ovly <- "base"
    return(base_reqs)
  } else if (inherits(base_reqs, "character")){
    # if base_reqs has no zoning requirements
    # return the overlay_reqs
    overlay_reqs$min_ovly <- overlay_type
    overlay_reqs$max_ovly <- overlay_type
    return(overlay_reqs)
  }


  min_val_list <- list()
  max_val_list <- list()
  min_ovly_list <- list()
  max_ovly_list <- list()
  if (overlay_type == "relax"){

    # loop through each constraint in the overlay reqs
    for (req_id in 1:nrow(overlay_reqs)){
      const_name <- overlay_reqs[req_id,"constraint_name"]  # the constraint name
      min_val_o <- overlay_reqs[req_id,"min_value"] |> unlist() # the min value of the constraint
      max_val_o <- overlay_reqs[req_id,"max_value"] |> unlist() # the max value of the constraint

      # get the min and max values for the base constraint
      min_val_b <- base_reqs[base_reqs$constraint_name == const_name,"min_value"] |> unlist()  # the min value of the constraint
      max_val_b <- base_reqs[base_reqs$constraint_name == const_name,"max_value"] |> unlist()  # the max value of the constraint

      min_of_mins <- ifelse(sum(!is.na(min_val_o)) > 0 | sum(!is.na(min_val_b)) > 0,
                            min(c(min_val_o, min_val_b), na.rm = T), NA)
      max_of_maxs <- ifelse(sum(!is.na(max_val_o)) | sum(!is.na(max_val_b)),
                            max(c(max_val_o, max_val_b), na.rm = T), NA)

      # min values
      if (length(min_val_b) > 1 & min_of_mins %in% min_val_b){
        # if the base district has ambiguous constraints,
        # then we need to check and see if either of its constraints
        # would be chosen as the relaxed value
        # if so, the new value will still be ambiguous, so we
        # select the two lowest values of the three compared
        nums <- c(min_val_o, min_val_b)
        new_val_min <- nums[! nums %in% max(nums)]
        # adding an overlay type to help with future code
        min_ovly_val <- ifelse(sum(new_val_min %in% min_val_b) == length(new_val_min), "base", overlay_type)
      } else{
        # if the relaxed value is not one of the constraint values of the
        # ambiguous base district, then it will simply return the
        # one relaxed value. This assumes that no overlay districts have
        # ambiguous values.
        new_val_min <- min_of_mins
        # adding an overlay type to help with future code
        min_ovly_val <- ifelse(min_of_mins == min_val_o, overlay_type, "base")
      }

      # max values
      if (length(max_val_b) > 1 & max_of_maxs %in% max_val_b){
        # if the base district has ambiguous constraints,
        # then we need to check and see if either of its constraints
        # would be chosen as the relaxed value
        # if so, the new value will still be ambiguous, so we
        # select the two highest values of the three compared
        nums <- c(max_val_o, max_val_b)
        new_val_max <- nums[! nums %in% min(nums)]
        # adding an overlay type to help with future code
        max_ovly_val <- ifelse(sum(new_val_max %in% max_val_b) == length(new_val_max), "base", overlay_type)
      } else{
        # if the relaxed value is not one of the constraint values of the
        # ambiguous base district, then it will simply return the
        # one relaxed value. This assumes that no overlay districts have
        # ambiguous values.
        new_val_max <- max_of_maxs
        # adding an overlay type to help with future code
        max_ovly_val <- ifelse(max_of_maxs == max_val_o, overlay_type, "base")
      }
      # assigning the new value to the the base requirements
      # note that the new value may be the same as it already was
      # but we replace it for convenience
      min_val_list[[req_id]] <- new_val_min
      max_val_list[[req_id]] <- new_val_max
      min_ovly_list[[req_id]] <- min_ovly_val
      max_ovly_list[[req_id]] <- max_ovly_val
    }

  } else if (overlay_type == "restrict"){

    # loop through each constraint in the overlay reqs
    for (req_id in 1:nrow(overlay_reqs)){
      const_name <- overlay_reqs[req_id,"constraint_name"]  # the constraint name
      min_val_o <- overlay_reqs[req_id,"min_value"] |> unlist() # the min value of the constraint
      max_val_o <- overlay_reqs[req_id,"max_value"] |> unlist() # the max value of the constraint

      # get the min and max values for the base constraint
      min_val_b <- base_reqs[base_reqs$constraint_name == const_name,"min_value"] |> unlist()  # the min value of the constraint
      max_val_b <- base_reqs[base_reqs$constraint_name == const_name,"max_value"] |> unlist()  # the max value of the constraint

      max_of_mins <- ifelse(sum(!is.na(min_val_o)) > 0 | sum(!is.na(min_val_b)) > 0,
                            max(c(min_val_o, min_val_b), na.rm = T), NA)
      min_of_maxs <- ifelse(sum(!is.na(max_val_o)) | sum(!is.na(max_val_b)),
                            min(c(max_val_o, max_val_b), na.rm = T), NA)

      # min values
      if (length(min_val_b) > 1 & max_of_mins %in% min_val_b){
        # if the base district has ambiguous constraints,
        # then we need to check and see if either of its constraints
        # would be chosen as the restricted value
        # if so, the new value will still be ambiguous, so we
        # select the two highest values of the three compared
        nums <- c(min_val_o, min_val_b)
        new_val_min <- nums[! nums %in% min(nums)]
        # adding an overlay type to help with future code
        min_ovly_val <- ifelse(sum(new_val_min %in% min_val_b) == length(new_val_min), "base", overlay_type)
      } else{
        # if the restricted value is not one of the constraint values of the
        # ambiguous base district, then it will simply return the
        # one restricted value. This assumes that no overlay districts have
        # ambiguous values.
        new_val_min <- max_of_mins
        # adding an overlay type to help with future code
        min_ovly_val <- ifelse(max_of_mins == min_val_o, overlay_type, "base")
      }

      # max values
      if (length(max_val_b) > 1 & min_of_maxs %in% max_val_b){
        # if the base district has ambiguous constraints,
        # then we need to check and see if either of its constraints
        # would be chosen as the restricted value
        # if so, the new value will still be ambiguous, so we
        # select the two lowest values of the three compared
        nums <- c(max_val_o, max_val_b)
        new_val_max <- nums[! nums %in% max(nums)]
        # adding an overlay type to help with future code
        max_ovly_val <- ifelse(sum(new_val_max %in% max_val_b) == length(new_val_max), "base", overlay_type)
      } else{
        # if the restricted value is not one of the constraint values of the
        # ambiguous base district, then it will simply return the
        # one restricted value. This assumes that no overlay districts have
        # ambiguous values.
        new_val_max <- min_of_maxs
        # adding an overlay type to help with future code
        max_ovly_val <- ifelse(min_of_maxs == max_val_o, overlay_type, "base")
      }
      # assigning the new value to the the base requirements
      # note that the new value may be the same as it already was
      # but we replace it for convenience
      min_val_list[[req_id]] <- new_val_min
      max_val_list[[req_id]] <- new_val_max
      min_ovly_list[[req_id]] <- min_ovly_val
      max_ovly_list[[req_id]] <- max_ovly_val
    }


  } else if (overlay_type == "replace"){
    # loop through each constraint in the overlay reqs
    for (req_id in 1:nrow(overlay_reqs)){
      const_name <- overlay_reqs[req_id,"constraint_name"]  # the constraint name
      new_val_min <- overlay_reqs[req_id,"min_value"] |> unlist() # the min value of the constraint
      new_val_max <- overlay_reqs[req_id,"max_value"] |> unlist() # the max value of the constraint
      base_min_val <- base_reqs[base_reqs$constraint_name == const_name, "min_value"] |> unlist()
      base_max_val <- base_reqs[base_reqs$constraint_name == const_name, "max_value"] |> unlist()

      # assigning the new value to the base requirements

      if (sum(!is.na(new_val_min)) > 0){
        # if there is a min value in the overlay constraint, it will replace the base value
        min_val_list[[req_id]] <- new_val_min
        min_ovly_list[[req_id]] <- overlay_type
      } else{
        # if not, it sill become the base value
        min_val_list[[req_id]] <- NA
        min_ovly_list[[req_id]] <- "base"
      }

      if (sum(!is.na(new_val_max)) > 0){
        # if there is a max value in the overlay constraint, it will replace the base value
        max_val_list[[req_id]] <- new_val_max
        max_ovly_list[[req_id]] <- overlay_type

        # base_reqs[base_reqs$constraint_name == const_name,"max_value"] <- new_val_max
        # base_reqs[base_reqs$constraint_name == const_name,"max_ovly"] <- overlay_type
      } else{
        max_val_list[[req_id]] <- NA
        max_ovly_list[[req_id]] <- "base"
      }
    }

  }

  # making a new data frame with all the constraints from the overlays
  # and the selected values based on the overlay type
  updated_reqs <- data.frame(constraint_name = overlay_reqs$constraint_name,
                             min_value = I(min_val_list),
                             max_value = I(max_val_list),
                             min_ovly = I(min_ovly_list),
                             max_ovly = I(max_ovly_list))

  # getting rid of the rows that will be replaced by the rows just
  # created from the overlay check
  base_reqs <- base_reqs[!base_reqs$constraint_name %in% updated_reqs$constraint_name,]

  # adding the overlay constraints to the base constraints
  base_reqs <- dplyr::bind_rows(base_reqs, updated_reqs)

  # replacing any NULL cells with NA to be consistent with formatting
  base_reqs[base_reqs == "NULL"] <- NA

  # after looping through each constraint in the one comparison scenario,
  # we return the new constraints df
  return(base_reqs)
}
