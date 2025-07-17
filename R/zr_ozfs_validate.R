#' Find invalid ozfs zoning files
#'
#' `zr_ozfs_validate()` lists any errors in the formatting of the OZFS data
#'
#' @param list_of_files A list of .zoning file paths
#'
#' @returns A list of files names with ozfs errors or warnings
#' @export
#'
zr_ozfs_validate <- function(list_of_files){

  # start the empty list where we will store errors
  error_list <- list()

  # loop through each file
  for (file in list_of_files){
    list_of_errors <- c() # can have multiple per file. will combine at the end
    list_of_warnings <- c()

    file_name <- basename(file)

    # CHECK GEOJSON COMPATIBILITY #
    ozfs_listed <- tryCatch(
      {
        rjson::fromJSON(file = file)
      }, error = function(e) {
        # Code to run if an error occurs
        return("error")
      }
    )

    if (inherits(ozfs_listed,"character")){
      error_list[[file_name]]$errors <- "Not geojson format"
      next
    }

    # CHECK DEFINITIONS #

    if (!"height" %in% names(ozfs_listed$definitions)){
      list_of_warnings <- c(list_of_warnings, "no height def.")
    }

    if (!"res_type" %in% names(ozfs_listed$definitions)){
      list_of_warnings <- c(list_of_errors, "no res_type def.")
    }

    # CHECK FEATURES #
    # loop through each feature
    for (feature_idx in 1:length(ozfs_listed$features)){

      properties <- ozfs_listed$features[[feature_idx]]$properties

      if (is.null(properties$dist_abbr)){ # check for dist_abbr
        dist_abbr <- feature_idx # variable to use in the warnings
        list_of_warnings <- c(list_of_warnings, paste(dist_abbr,": no dist_abbr field"))
      } else{
        dist_abbr <- properties$dist_abbr # variable to use in warnings
      }
      if (is.null(properties$planned_dev)){ # check for planned_dev
        list_of_warnings <- c(list_of_warnings, paste(dist_abbr,": no planned_dev field"))
      }
      if (is.null(properties$overlay)){ # check for overlay
        list_of_warnings <- c(list_of_warnings, paste(dist_abbr,": no overlay field"))
      }

      # if there are constraints, then it will
      # save them as a variable to loop through later
      if (!is.null(properties$constraints)){
        constraints <- properties$constraints
      } else{
        next
      }

      # possible constraint names that could be listed
      possible_constraint_names <- c("lot_area",
                                     "setback_front",
                                     "setback_side_int",
                                     "setback_side_ext",
                                     "setback_rear",
                                     "setback_side_sum",
                                     "setback_front_sum",
                                     "setback_dist_boundary",
                                     "lot_cov_bldg",
                                     "parking_enclosed",
                                     "parking_covered",
                                     "parking_uncovered",
                                     "stories",
                                     "height",
                                     "height_eave",
                                     "unit_size",
                                     "unit_size_avg",
                                     "unit_density",
                                     "total_units",
                                     "units_0bed",
                                     "units_1bed",
                                     "units_2bed",
                                     "units_3bed",
                                     "units_4bed",
                                     "unit_pct_0bed",
                                     "unit_pct_1bed",
                                     "unit_pct_2bed",
                                     "unit_pct_3bed",
                                     "unit_pct_4bed",
                                     "footprint",
                                     "fl_area",
                                     "fl_area_first",
                                     "fl_area_top",
                                     "far")

      # possible variables that could be used in expressions
      # given a random value just so they will be evaluated correctly
      lot_width <- 1
      lot_depth <- 1
      lot_area <- 1
      lot_type <- "corner"
      bedrooms <- 1
      total_bedrooms <- 1
      units_0bed <- 1
      units_1bed <- 1
      units_2bed <- 1
      units_3bed <- 1
      units_4bed <- 1
      total_units <- 1
      fl_area <- 1
      fl_area_bottom <- 1
      parking_covered <- 1
      parking_uncovered <- 1
      parking_enclosed <- 1
      parking_floors <- 1
      parking_bel_grade <- "yes"
      garage_entry <- "front"
      height <- 1
      height_eave <- 1
      floors <- 1
      min_unit_size <- 1
      max_unit_size <- 1
      far <- 1
      bldg_width <- 1
      bldg_dpth <- 1
      level_units_table <- 1
      units_floor1 <- 1
      units_floor2 <- 1
      units_floor3 <- 1
      res_type <- "1_unit"


      # stating which constraints are written properly
      proper_constraint_names <- names(constraints) %in% possible_constraint_names

      # if there are any constraints not written correctly,
      # a warning note is given
      if (FALSE %in% proper_constraint_names){
        incorrect_constraint_names <- names(constraints)[proper_constraint_names == FALSE]
        list_of_warnings <- c(list_of_warnings, paste("bad constraint names:",paste(incorrect_constraint_names, collapse = ", ")))
      }

      # CHECK INDIVIDUAL CONSTRAINTS #
      # loop through each constraint
      for (constraint_idx in 1:length(constraints)){
        constraint_name <- names(constraints)[[constraint_idx]]
        constraint_list <- constraints[[constraint_idx]]

        if (!is.null(constraint_list$min_val) & !is.null(constraint_list$max_val)){
          minmax_vals <- c("min_val","max_val")
        } else if (!is.null(constraint_list$min_val)){
          minmax_vals <- "min_val"
        } else if (!is.null(constraint_list$max_val)){
          minmax_vals <- "max_val"
        } else{
          list_of_errors <- c(list_of_errors, paste(dist_abbr,": no min or max vals for",constraint_name))
        }

        # Loop through the value lists
        for (minmax_idx in 1:length(minmax_vals)){
          minmax_name <- minmax_vals[[minmax_idx]]
          minmax <- constraint_list[[minmax_name]]


          # Loop through each item in the value list
          for (condition_num in 1:length(minmax)){
            val_list <- minmax[[condition_num]]

            if (is.null(val_list$expression)){ # expression field is required
              list_of_errors <- c(list_of_errors, paste(dist_abbr,": no expression for",minmax_name, "of",constraint_name))
            } else{
              parsed_expression <- lapply(val_list$expression, function(x){
                tryCatch(
                  {
                    parse(text = x)
                  }, error = function(e) {
                    # Code to run if an error occurs
                    return(NULL)
                  }
                )
              })

              if (is.null(parsed_expression)){ # # expressions must parse properly
                list_of_errors <- c(list_of_errors, paste(dist_abbr,": expression parse error for",minmax_name, "of",constraint_name))
              } else{
                eval_expression <- lapply(parsed_expression, function(x){
                  tryCatch(
                    {
                      eval(x)
                    }, error = function(e) {
                      # Code to run if an error occurs
                      return(NULL)
                    }
                  )
                } ) |> unlist()
                if (is.null(eval_expression)){ # expressions must evaluate properly
                  list_of_errors <- c(list_of_errors, paste(dist_abbr,": expression eval error for",minmax_name, "of",constraint_name))
                }
                if (sum(is.na(eval_expression)) > 0 ){ # NAs shouldn't exist
                  list_of_errors <- c(list_of_errors, paste(dist_abbr,": NA expression encountered for",minmax_name, "of",constraint_name))
                }
              }
            }

            if (!is.null(val_list$criterion)){
              if (!val_list$criterion %in% c("min","max")){
                # if criterion is something besides "min" or "max" it will cause en error
                list_of_errors <- c(list_of_errors, paste(dist_abbr,": improper criterion for",minmax_name, "of",constraint_name))
              }
              if (length(val_list$expression) == 1){
                # if there is a criterion, there should be multiple values in expression
                list_of_warnings <- c(list_of_warnings, paste(dist_abbr,": criterion not needed",minmax_name, "of",constraint_name))
              }
            }

            if (!is.null(val_list$condition)){
              eval_condition <- lapply(val_list$condition, function(x){
                tryCatch(
                  {
                    eval(parse(text = x))
                  }, error = function(e) {
                    # Code to run if an error occurs
                    return(NULL)
                  }
                )
              } ) |> unlist()

              if (sum(is.na(eval_condition)) > 0){ # NAs shouldn't exist
                list_of_errors <- c(list_of_errors, paste(dist_abbr,": NA condition encountered for",minmax_name, "of",constraint_name))
              }

            }


          } # end loop through each item in the value list

        } # end loop through the value lists

      } # end loop through each constraint

    } # end loop through each feature



    if (length(list_of_warnings) > 0){
      error_list[[file_name]]$warnings <- list_of_warnings
    }
    if (length(list_of_errors) > 0){
      error_list[[file_name]]$errors <- list_of_errors
    }

  } # end loop through each file


  if (length(error_list) == 0){
    return(cat("Files appear to follow proper OZFS standards \n"))
  } else if (length(error_list) == 1){
    cat("There was",paste(length(error_list), "file with errors or warnings \n\n"))
  } else{
    cat("There were",paste(length(error_list), "files with errors or warnings \n\n"))
  }

  return(error_list)
}
