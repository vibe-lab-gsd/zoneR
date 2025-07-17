#' List district zoning requirement values
#'
#' Because many zoning requirements depend on the proposed building
#' or the parcel of that zoning district, the `zr_get_zoning_req()` function
#' takes a tidybuilding, a tidyparcel, and a district_data and outputs a data
#' frame listing the set zoning requirements that those three objects would create.
#' If every value is NA, it could indicate that the building
#' land use is not allowed in the zoning district.
#'
#' @param bldg_data either the file path to an OZFS *.bldg file or
#' a list created from the the *.bldg file using `rjson::fromJSON`
#' @param parcel_data one row of a parcel data frame created from the
#' OZFS *.parcel file
#' @param district_data one row (representing one district) of a
#' zoning data frame created from the OZFS *.zoning file
#' @param zoning_data either the path to a *.zoning file or
#' a list created from the the *.zoning file using `rjson::fromJSON`
#' @param vars the result from the `get_variables()` function.
#' If this data frame is supplied, bldg_data, parcel_data, and zoning_data
#' are not needed.
#'
#' @return
#' Returns a data frame with the value of each zoning requirement for that specific building, parcel, and zoning district.
#' NA values indicate the requirement isn't recorded in that district.
#' If every value is NA, it could indicate that the building land use is not allowed in the zoning district.
#' @export
#'
zr_get_zoning_req <- function(district_data,
                           bldg_data = NULL,
                           parcel_data = NULL,
                           zoning_data = NULL,
                           vars = NULL){

  # check to see if there are any constraints in the district
  if (is.null(district_data$constraints)){
    return("No zoning requirements recorded for this district")
  } else if (is.na(district_data$constraints)){
    return("No zoning requirements recorded for this district")
  }

  listed_constraints <- rjson::fromJSON(district_data$constraints)
  constraints <- names(listed_constraints)
  constraints <- constraints[constraints != "unit_size"]

  if (length(constraints) == 0){
    return("No zoning requirements recorded for this district")
  }


  if (is.null(vars)){
    vars <- zr_get_variables(bldg_data, parcel_data, district_data, zoning_data)
  }

  for (var_idx in 1:ncol(vars)){
    vname <- names(vars)[[var_idx]]
    assign(vname, vars[[1,vname]])
  }


  # loop through each zoning regulation in the district
  min_vals <- rep(list(NA), length(constraints))
  min_val_notes <- rep(list(NA), length(constraints))
  max_vals <- rep(list(NA), length(constraints))
  max_val_notes <- rep(list(NA), length(constraints))
  for (i in 1:length(constraints)){
    constraint_name <- constraints[[i]]
    constraint_list <- listed_constraints[[constraint_name]]

    # loop through each min/max_val
    for (val_idx in 1:length(constraint_list)){
      val_name <- names(constraint_list)[[val_idx]]
      val_list <- constraint_list[[val_name]]

      value <- NA
      note <- NA

      # loop through each array item under the variable
      maybe_ids <- c()
      true_id <- NULL
      for (j in 1:length(val_list)){
        condition_list <- val_list[[j]]$condition

        # When there is just one array item
        if (length(val_list) == 1){
          true_id <- j
          break
        }

        condition_list <- lapply(condition_list, function(x) gsub("\\band\\b", "&", x))
        condition_list <- lapply(condition_list, function(x) gsub("\\bor\\b", "|", x))
        eval_conditions <- sapply(condition_list, function(x) {
          tryCatch({
          eval(parse(text = x))
        }, error = function(e) {
          return("MAYBE")
        })
          })

        if (sum(eval_conditions == "TRUE") == length(eval_conditions)){ # all conditions == TRUE
          true_id <- j
          break
        } else if ("FALSE" %in% eval_conditions){ # there's at least one FALSE
          next
        } else{ # has MAYBE with perhaps some TRUE
          maybe_ids <- c(maybe_ids, j)
        }


      }# end loop through each array item under the variable

      if (!is.null(true_id)){
        expressions <- val_list[[true_id]]$expression
        eval_expressions <- lapply(expressions, function(x){
          tryCatch({
            eval(parse(text = x))
          }, error = function(e) {
            return(NULL)
          })
        }) |> unlist()

        if (is.null(eval_expressions)){
          note <- "Unable to evaluate expression: incorrect format or missing variables"
        } else if (length(eval_expressions) > 1){
          if (!is.null(val_list[[j]]$min_max)){
            value <- ifelse(val_list[[j]]$min_max == "min",min(eval_expressions), max(eval_expressions))
          } else if (min(eval_expressions) == max(eval_expressions)){
            value <- min(eval_expressions)
          } else{
            value <- c(min(eval_expressions), max(eval_expressions))
          }
        } else{
          value <- eval_expressions
        }
      } else if (!is.null(maybe_ids)){
        possible_vals <- c()
        for (id in maybe_ids){
          expressions <- val_list[[id]]$expression
          eval_expressions <- lapply(expressions, function(x){
            tryCatch({
              eval(parse(text = x))
            }, error = function(e) {
              return(NULL)
            })
          }) |> unlist()

          if (is.null(eval_expressions)){
            note <- "Unable to evaluate expression: incorrect format or missing variables"
          } else if (length(eval_expressions) > 1){
            if (!is.null(val_list[[j]]$min_max)){
              possible_vals <- c(possible_vals, ifelse(val_list[[j]]$min_max == "min",min(eval_expressions), max(eval_expressions)))
            } else{
              possible_vals <- c(possible_vals, c(min(eval_expressions), max(eval_expressions)))
            }
          } else{
            possible_vals <- c(possible_vals, eval_expressions)
          }
        }

        if (min(possible_vals) == max(possible_vals)){
          value <- min(possible_vals)
        } else{
          value <- c(min(possible_vals), max(possible_vals))
        }

      } else{
        note <- "No constraint conditions met"
      }

      if (val_name == "min_val"){
        min_vals[[i]] <- round(value, 4)
        min_val_notes[[i]] <- note
      } else{
        max_vals[[i]] <- round(value,4)
        max_val_notes[[i]] <- note
      }



    }# end loop through each min/max_val

  }# end loop through each constraint


  # put everything into a data frame
  constraints_df <- data.frame(constraint_name = constraints,
                               min_value = I(min_vals),
                               max_value = I(max_vals),
                               min_val_error = I(min_val_notes),
                               max_val_error = I(max_val_notes))


  return(constraints_df)
}

