#' Compare unit sizes and allowed unit sizes
#'
#' `zr_check_unit()` checks to see if the district's zoning
#' code allows the building based on unit sizes.
#'
#' @param district_data one row (representing one district) of a
#' zoning data frame created from the OZFS *.zoning file
#' @param bldg_data either the file path to an OZFS *.bldg file or
#' a list created from the the *.bldg file using `rjson::fromJSON`
#' @param vars the result from the `get_variables()` function.
#' If this data frame is supplied, bldg_data, parcel_data, and zoning_data
#' are not needed.
#'
#' @return
#' Returns TRUE, FALSE, or MAYBE stating whether or not the building would
#' be allowed in the district based on the unit sizes.
#' @export
#'
zr_check_unit <- function(district_data, bldg_data, vars){

  # check to see if there are any constraints in the district
  if (is.null(district_data$constraints)){
    return(TRUE)
  } else if (is.na(district_data$constraints)){
    return(TRUE)
  }

  listed_constraints <- rjson::fromJSON(district_data$constraints)

  if (length(listed_constraints) == 0){
    return(TRUE)
  }

  unit_info <- zr_get_unit_info(bldg_data)

  # if no bedrooms area recorded, give a warning, and assume 2 beds per unit.
  if (is.null(unit_info$bedrooms)){
    warning("No bedroom qty recorded in the tidybuilding. Results may be innacurate")
    unit_info$bedrooms <- 2
  }

  for (var_idx in 1:ncol(vars)){
    vname <- names(vars)[[var_idx]]
    assign(vname, vars[[1,vname]])
  }

  # getting a df with just the min and max of each unit type
  unit_info_df <- unit_info |>
    dplyr::group_by(bedrooms) |>
    dplyr::summarise(min = min(fl_area),
                     max = max(fl_area))

  constraint_name <- "unit_size"
  constraint_list <- listed_constraints[[constraint_name]]


  min_vals <- rep(0, nrow(unit_info_df))
  min_val_notes <- rep(list(NA), nrow(unit_info_df))
  max_vals <- rep(100000, nrow(unit_info_df))
  max_val_notes <- rep(list(NA), nrow(unit_info_df))
  # loop through each row in unit_info_df
  for (i in 1:nrow(unit_info_df)){
    bedrooms <- unit_info_df$bedrooms[[i]]
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

  } # end loop through each row in unit_info_df

  unit_info_df$min_val <- min_vals
  unit_info_df$max_val <- max_vals
  unit_info_df$min_ok <- NA
  unit_info_df$max_ok <- NA
  unit_info_df$permitted <- NA

  # unit_info_df <- unit_info_df[,1:5]
  #
  # unit_info_df |>
  #   dplyr::mutate(min_ok = min > min_val & min < max_val,
  #                 max_ok = max > min_val & max < max_val)

  for (i in 1:nrow(unit_info_df)){
    min_val <- unit_info_df[i,"min"][[1]][[1]]
    max_val <- unit_info_df[i,"max"][[1]][[1]]
    min_req <- unit_info_df[i,"min_val"][[1]][[1]]
    max_req <- unit_info_df[i,"max_val"][[1]][[1]]

    if (length(min_req) > 1){
      min_min_check1 <- min_val >= min_req[[1]]
      min_min_check2 <- min_val >= min_req[[2]]
      max_min_check1 <- max_val >= min_req[[1]]
      max_min_check2 <- max_val >= min_req[[2]]

      sum_min_min_check <- min_min_check1 + min_min_check2
      sum_max_min_check <- max_min_check1 + max_min_check2

      sum_min_checks <- sum_min_min_check + sum_max_min_check

      if (sum_min_min_check == 0 | sum_max_min_check == 0){
        min_check <- FALSE
      } else if(sum_min_min_check == 2 | sum_max_min_check == 2){
        min_check <- TRUE
      } else{
        min_check <- "MAYBE"
      }

    } else{
      min_check <- min_val >= min_req & max_val >= min_req
    }

    if (length(max_req) > 1){
      min_min_check1 <- min_val <= max_req[[1]]
      min_min_check2 <- min_val <= max_req[[2]]
      max_min_check1 <- max_val <= max_req[[1]]
      max_min_check2 <- max_val <= max_req[[2]]

      sum_min_min_check <- min_min_check1 + min_min_check2
      sum_max_min_check <- max_min_check1 + max_min_check2

      sum_min_checks <- sum_min_min_check + sum_max_min_check

      if (sum_min_min_check == 0 | sum_max_min_check == 0){
        max_check <- FALSE
      } else if(sum_min_min_check == 2 | sum_max_min_check == 2){
        max_check <- TRUE
      } else{
        max_check <- "MAYBE"
      }

    } else{
      max_check <- min_val <= max_req & max_val <= max_req
    }


    if (min_check == FALSE | max_check == FALSE){
      permitted <- FALSE
    } else if(min_check == TRUE & max_check == TRUE){
      permitted <- TRUE
    } else{
      permitted <- "MAYBE"
    }


    unit_info_df$min_ok[[i]] <- min_check
    unit_info_df$max_ok[[i]] <- max_check
    unit_info_df$permitted[[i]] <- permitted

  }

  permitted_col <- unique(unit_info_df$permitted)

  if (FALSE %in% permitted_col){
    unit_size_check <- FALSE
  } else if ("MAYBE" %in% permitted_col){
    unit_size_check <- "MAYBE"
  } else{
    unit_size_check <- TRUE
  }

  return(unit_size_check)
}
