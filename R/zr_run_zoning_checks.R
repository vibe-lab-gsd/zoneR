#' Find where a building is allowed to be built
#'
#' `zr_run_zoning_checks()` checks the building information against all the
#' zoning constraints to see which parcels will allow the building.
#'
#' @param bldg_file The path to the OZFS *.bldg file
#' @param parcels_file The path to the OZFS *.parcel file
#' @param zoning_file The path to the OZFS *.zoning
#' @param detailed_check When TRUE, every parcel passes through each
#' check no matter the result, and it take more time. When FALSE,
#' subsequent checks are skipped as soon as one check reads FALSE
#' @param print_checkpoints When TRUE, runtimes and other info will be
#' printed at certain points throughout the function.
#' @param checks A list of all the checks that should take place. The default is
#' to check for every constraint possible in the OZFS. These constraints can found
#' in the package data `possible_checks`. Note, if a zoning file doesn't have zoning
#' info for one of the constraints listed in the checks variable, then it is
#' assumed that building characteristic is allowed.
#'
#' @returns a simple features data frame with the centroid of each parcel with a column
#' stating building allowance on the parcel and a column stating the reason
#' why certain parcels don't allow the building.
#' @export
#'
#' @examples
zr_run_zoning_checks <- function(bldg_file,
                                 parcels_file,
                                 zoning_file,
                                 detailed_check = FALSE,
                                 print_checkpoints = TRUE,
                                 checks = possible_checks){

  # track the start time to give a time stamp at end
  total_start_time <- proc.time()[[3]]

  incorrect_checks <- checks[!checks %in% possible_checks]

  if (length(incorrect_checks) > 0){
    warning(paste("Unknown constraints assigned to check input:",paste(incorrect_checks, collapse =", ")))
  }

  initial_checks <- checks[!checks %in% c("res_type",
                                       "unit_size",
                                       "bldg_fit",
                                       "overlay")]

  ########---- START DATA PREP----########
  ## the buildign json list ##
  bldg_data <- rjson::fromJSON(file = bldg_file)

  ## zoning data ##
  # get the full ozfs data as an sf data frame
  zoning_all_sf <- sf::st_read(zoning_file, quiet = TRUE)
  # get just the overlay districts with geometry
  overlays <- zoning_all_sf |>
    dplyr::filter(!sf::st_is_empty(geometry)) |>
    dplyr::filter(overlay == TRUE)
  # get just the pd_districts with geometry
  pd_districts <- zoning_all_sf |>
    dplyr::filter(!sf::st_is_empty(geometry)) |>
    dplyr::filter(planned_dev == TRUE)
  # get just the base districts with geometry
  # this is the one I will use for most of the checks
  zoning_sf <- zoning_all_sf |>
    dplyr::filter(!sf::st_is_empty(geometry)) |>
    dplyr::filter(overlay == FALSE) |>
    dplyr::filter(planned_dev == FALSE)

  #zoning_data is the json list form of the zoning file
  zoning_data <- rjson::fromJSON(file = zoning_file)

  # get appropriate crs in meters to use in the check footprint function
  crs <- zr_get_crs(zoning_sf)

  ## PARCELS ##
  # separate the parcel data into two special feature data frames
  parcel_geo <- zr_get_parcel_geo(parcels_file) # parcels with side labels
  parcel_dims <- zr_get_parcel_dims(parcels_file) # parcels with centroid and dimensions


  ## GET DISTRICT INDICES ##
  # use the base zoning districts to add zoning_id to parcel_dims
  parcel_dims <- zr_find_district_idx(parcel_dims, zoning_sf, "zoning_id")

  zoning_is_na <- parcel_dims$zoning_id |>
    unique() |>
    is.na()

  if (length(zoning_is_na) == 1 & TRUE %in% zoning_is_na){
    stop("Zoning districts and parcels do not appear to overlap.")
  }

  # add false_reasons and maybe_reasons columns to parcel_dims (for tracking maybes and falses)
  # this parcel_df is what we will use for most of the calculations
  parcel_df <- parcel_dims |>
    dplyr::mutate(false_reasons = as.character(NA),
                  maybe_reasons = as.character(NA))



  # start a list that will store the false data frames of the check functions
  false_df <- list()
  false_df_idx <- 1
  ########----END DATA PREP----########

  # print checkpoint info
  if (print_checkpoints){
    time_lapsed <- proc.time()[[3]] - total_start_time
    cat(ifelse(time_lapsed > 60,
           paste0("___data_prep___(",round(time_lapsed / 60,2), " min)\n\n"),
           paste0("___data_prep___(",round(time_lapsed,1), " sec)\n\n")))
  }
  ########----START CHECKS----########
  # PLANNED DEVELOPMENT CHECK
  pd_time <- proc.time()[[3]]
  # if parcels are in a planned development, the building is automatically not allowed
  if (nrow(pd_districts) > 0){ # if there are pd_districts
    # make a new df with the pd district indexes
    pd_parcel_df <- zr_find_district_idx(parcel_dims, pd_districts, "pd_id") |>
      dplyr::filter(!is.na(pd_id))

    pd_parcels <- unique(pd_parcel_df$parcel_id)

    parcel_df <- parcel_df |>
      dplyr::mutate(check_pd = ifelse(parcel_id %in% pd_parcels, FALSE, TRUE),
                    false_reasons = ifelse(parcel_id %in% pd_parcels, ifelse(!is.na(false_reasons),paste(false_reasons, "PD_dist", sep = ", "),"PD_dist"), false_reasons))

    # if detailed_check == FALSE, then we store the FALSE parcels in a list to be combined later
    # we filter the parcel_df to have just the TRUEs and MAYBEs
    # so it will be small for the next checks
    if (detailed_check == FALSE){
      false_parcels <- parcel_df |>
        dplyr::filter(check_pd == FALSE)
      # Add the false_parcels to the false_df list
      false_df[[false_df_idx]] <- false_parcels
      false_df_idx <- false_df_idx + 1

      parcel_df <- parcel_df |>
        dplyr::filter(check_pd == TRUE)
    }

    # print checkpoint info
    if (print_checkpoints){
      time_lapsed <- proc.time()[[3]] - pd_time
      cat(ifelse(time_lapsed > 60,
                 paste0("___planned_dev_check___(",round(time_lapsed / 60,2), " min)\n"),
                 paste0("___planned_dev_check___(",round(time_lapsed,1), " sec)\n")))
      cat(paste(length(parcel_df$zoning_id[parcel_df$check_pd == TRUE]),"parcels in planned developement district\n\n"))
    }

  }

  # GET ZONING REQUIREMENTS AND VARIABLES
  zone_req_var_time <- proc.time()[[3]]

  vars_list <- list()
  zoning_req_list <- list()
  for (row_num in 1:nrow(parcel_df)){
    parcel_data <- parcel_df[row_num,]
    parcel_id <- as.character(parcel_data$parcel_id)
    district_data <- zoning_sf[parcel_data$zoning_id,]
    vars <- zr_get_variables(bldg_data, parcel_data, district_data, zoning_data)
    zoning_req <- zr_get_zoning_req(district_data, vars = vars)

    vars_list[[parcel_id]] <- vars
    zoning_req_list[[parcel_id]] <- zoning_req
  }

  # print checkpoint info
  if (print_checkpoints){
    time_lapsed <- proc.time()[[3]] - zone_req_var_time
    cat(ifelse(time_lapsed > 60,
               paste0("___get_zoning_req___(",round(time_lapsed / 60,2), " min)\n\n"),
               paste0("___get_zoning_req___(",round(time_lapsed,1), " sec)\n\n")))
  }


  # INITIAL CHECKS
  # perform all the initial checks
  func_start_time <- proc.time()[[3]]

  # start empty variables to store potential errors and warnings
  errors <- c()
  warnings <- c()
  initial_checks_list <- list()
  true_maybe_list <- c()
  # loop through each parcel for initial checks
  for (i in 1:nrow(parcel_df)){
    parcel_data <- parcel_df[i,]
    district_data <- zoning_sf[parcel_data$zoning_id,]
    vars <- vars_list[[parcel_data$parcel_id]]
    zoning_req <- zoning_req_list[[parcel_data$parcel_id]]

    #checking res_type
    if ("res_type" %in% checks){
      res_type_check_df <- data.frame(res_type = as.character(zr_check_res_type(vars, district_data)))
    } else{
      res_type_check_df <- data.frame(row.names = 1)
    }

    #checking other initial constraints
    # we need to check for the unit size separately with zr_check_unit
    check_constraints_df <- tryCatch({
      zr_check_constraints(vars, zoning_req, initial_checks)
    }, error = function(e) {
      # code to execute for errors
      paste(FALSE)
    })


    if (inherits(check_constraints_df, "data.frame")){
      # Pivot to one row
      checks_df <- as.data.frame(t(check_constraints_df$allowed))
      colnames(checks_df) <- check_constraints_df$constraint_name
    } else if (inherits(check_constraints_df, "character")){
      checks_df <- data.frame(row.names = 1)
    } else{
      checks_df <- data.frame(row.names = 1)
      warning("there was an error in zr_check_constraints function")
    }

    checks_df[] <- lapply(checks_df, as.character)

    #checking res_type
    if ("unit_size" %in% checks){
      unit_check_df <- data.frame(unit_size = as.character(zr_check_unit(vars, district_data)))
    } else{
      unit_check_df <- data.frame(row.names = 1)
    }

    combined_checks <- cbind(res_type_check_df, checks_df, unit_check_df)

    # get a vector of each value in the combined_checks df
    value_vec <- as.character(unlist(combined_checks[1, ]))

    maybe_constraints <- names(combined_checks)[value_vec == "MAYBE"]
    false_constraints <- names(combined_checks)[value_vec == "FALSE"]

    # assign an overall value for the check
    if (FALSE %in% value_vec){
      check <- FALSE
    } else if ("MAYBE" %in% value_vec){
      check <- "MAYBE"
      true_maybe_list <- c(true_maybe_list,i)
    } else{
      check <- TRUE
      true_maybe_list <- c(true_maybe_list,i)
    }

    initial_checks_list[[i]] <- combined_checks

    # if the check returns FALSE or MAYBE,
    # then write the function name in the reasons column
    if (length(maybe_constraints) > 0){
      parcel_df[i,"maybe_reasons"] <- ifelse(is.na(parcel_df[[i,"maybe_reasons"]]), paste(maybe_constraints, collapse = ", "), paste(parcel_df[[i,"maybe_reasons"]], paste(maybe_constraints, collapse = ", "), sep = ", "))
    }

    if (length(false_constraints) > 0){
      parcel_df[i,"false_reasons"] <- ifelse(is.na(parcel_df[[i,"false_reasons"]]), paste(false_constraints, collapse = ", "), paste(parcel_df[[i,"false_reasons"]], paste(false_constraints, collapse = ", "), sep = ", "))
    }


  } # end loop through each parcel for initial checks


  all_initial_checks_df <- dplyr::bind_rows(initial_checks_list)

  # if there are any NAs, it means that was a constraint in other districts
  # but not the one this parcel is in
  # We assume if it didn't specify a constraint, then the building is allowed
  # and we change the NAs to TRUE
  all_initial_checks_df[is.na(all_initial_checks_df)] <- TRUE

  parcel_df <- cbind(parcel_df, all_initial_checks_df)

  # if detailed_check == FALSE, then we store the FALSE parcels in a list to be combined later
  # we filter the parcel_df to have just the TRUEs and MAYBEs
  # so it will be smaller for the next checks
  if (detailed_check == FALSE){
    false_parcels <- parcel_df[!is.na(parcel_df$false_reasons),]
    parcel_df <- parcel_df[is.na(parcel_df$false_reasons),]
    # Add the false_parcels to the false_df list
    false_df[[false_df_idx]] <- false_parcels
    false_df_idx <- false_df_idx + 1
  }

  # print checkpoint info
  if (print_checkpoints){
    time_lapsed <- proc.time()[[3]] - func_start_time
    cat(ifelse(time_lapsed > 60,
               paste0("___initial_checks___(",round(time_lapsed / 60,2), " min)\n"),
               paste0("___initial_checks___(",round(time_lapsed,1), " sec)\n")))
    cat(paste(length(true_maybe_list),"parcels are TRUE or MAYBE\n\n"))
  }

  ###### IN PROGRESS
  zoning_req_empty <- sapply(zoning_req_list, function(x) inherits(x,"character"))
############# IN PROGRESS

  # SIDE LABEL CHECK
  # if parcels have labeled sides, we can move on to the footprint check
  if ("bldg_fit" %in% checks & nrow(parcel_df) > 0){
    parcels_with_sides <- unique(parcel_geo$parcel_id)

    parcel_df <- parcel_df |>
      dplyr::mutate(parcel_side_lbl = ifelse(parcel_id %in% parcels_with_sides,TRUE, "MAYBE"),
                    maybe_reasons = ifelse(parcel_id %in% parcels_with_sides, maybe_reasons, ifelse(!is.na(maybe_reasons),paste(maybe_reasons, "side_lbl", sep = ", "),"side_lbl")))

    parcel_no_sides <- parcel_df |>
      dplyr::filter(!parcel_id %in% parcels_with_sides)

    false_df[[false_df_idx]] <- parcel_no_sides
    false_df_idx <- false_df_idx + 1

    parcel_df <- parcel_df |>
      dplyr::filter(parcel_id %in% parcels_with_sides)
  }


  # FIT CHECK
  # see if the building footprint fits in the parcel's buildable area

  if ("bldg_fit" %in% checks & nrow(parcel_df) > 0 & !is.null(parcel_geo)){
    foot_start_time <- proc.time()[[3]]
    for (z in 1:nrow(parcel_df)){
      parcel_data <- parcel_df[z,]
      district_data <- zoning_sf[parcel_data$zoning_id,]
      zoning_req <- zoning_req_list[[parcel_data$parcel_id]]
      vars <- vars_list[[parcel_data$parcel_id]]

      # if the footprint area is smaller than the parcel area,
      # then run the check_fit function
      if (vars$lot_cov_bldg <= 100){
        parcel_sides <- parcel_geo |>
          dplyr::filter(parcel_id == parcel_data$parcel_id)
        parcel_with_setbacks <- zr_add_setbacks(parcel_sides, district_data, zoning_req)
        buildable_area <- zr_get_buildable_area(parcel_with_setbacks, crs)

        # if two buildable areas were recorded, we need to test for both
        if (length(buildable_area) > 1){
          check_1 <- zr_check_fit(bldg_data, sf::st_make_valid(buildable_area[[1]]), crs = crs)

          if (check_1){
            check <- check_1
          } else{
            check_2 <- zr_check_fit(bldg_data, sf::st_make_valid(buildable_area[[2]]), crs = crs)
            if (check_2){
              check <- "MAYBE"
            } else{
              check <- FALSE
            }
          }

        } else{
          check <- zr_check_fit(bldg_data, sf::st_make_valid(buildable_area[[1]]), crs = crs)
        }

      } else{
        check <- FALSE
      }

      parcel_df[z, "bldg_fit"] <- as.character(check)

      # if the check returns FALSE or MAYBE,
      # then write the function name in the reasons column
      if (check == "MAYBE"){
        parcel_df[z,"maybe_reasons"] <- ifelse(is.na(parcel_df[[z,"maybe_reasons"]]), "bldg_fit", paste(parcel_df[[z,"maybe_reasons"]], "bldg_fit", sep = ", "))
      }

      if (check == FALSE){
        parcel_df[z,"false_reasons"] <- ifelse(is.na(parcel_df[[z,"false_reasons"]]), "bldg_fit", paste(parcel_df[[z,"false_reasons"]], "bldg_fit", sep = ", "))
      }
    }

    # if detailed_check == FALSE, then we store the FALSE parcels in a list to be combined later
    # we filter the parcel_df to have just the TRUEs and MAYBEs
    # so it will be smalle for the next checks
    if (detailed_check == FALSE){
      false_parcels <- parcel_df[parcel_df[,"bldg_fit"][[1]] == FALSE,]
      parcel_df <- parcel_df[parcel_df[,"bldg_fit"][[1]] %in% c(TRUE, "MAYBE"),]
      # Add the false_parcels to the false_df list
      false_df[[false_df_idx]] <- false_parcels
      false_df_idx <- false_df_idx + 1
    }

    # print checkpoint info
    if (print_checkpoints){
      time_lapsed <- proc.time()[[3]] - foot_start_time
      cat(ifelse(time_lapsed > 60,
                 paste0("___bldg_fit___(",round(time_lapsed / 60,2), " min)\n"),
                 paste0("___bldg_fit___(",round(time_lapsed,1), " sec)\n")))
      cat(paste(length(which(parcel_df[,"bldg_fit"][[1]] %in% c(TRUE, 'MAYBE'))),"parcels are TRUE or MAYBE\n\n"))
    }

  }

  # OVERLAY CHECK
  overlay_time <- proc.time()[[3]]
  # of the parcels that pass all the checks,
  # the ones in an overlay district will be marked as "MAYBE"
  if (nrow(overlays) > 0 & "overlay" %in% checks){ # if there are pd_districts
    # make a new df with the overlay district indexes
    parcels_overlays <- zr_find_district_idx(parcel_dims, overlays, "overlay_id") |>
      dplyr::filter(!is.na(overlay_id))

    overlay_parcels <- unique(parcels_overlays$parcel_id)

    parcel_df <- parcel_df |>
      dplyr::mutate(check_overlay = ifelse(parcel_id %in% overlay_parcels,"MAYBE", TRUE),
                    maybe_reasons = ifelse(parcel_id %in% overlay_parcels, ifelse(!is.na(maybe_reasons),paste(maybe_reasons, "parcel in overlay district", sep = ", "),"parcel in overlay district"), maybe_reasons))

    # print checkpoint info
    if (print_checkpoints){
      time_lapsed <- proc.time()[[3]] - overlay_time
      cat(ifelse(time_lapsed > 60,
                 paste0("___overlay_check___(",round(time_lapsed / 60,2), " min)\n"),
                 paste0("___overlay_check___(",round(time_lapsed,1), " sec)\n")))
      cat(paste(length(parcel_df$zoning_id[parcel_df$check_overlay == TRUE]),"parcels in overlay districts\n\n"))
    }
  }
  ########----END CHECKS----########


  ########----FINALIZING THINGS----########
  # combind all the false_df and the parcel_df
  class(parcel_df$false_reasons) <- "character"
  class(parcel_df$maybe_reasons) <- "character"
  final_df <- dplyr::bind_rows(false_df, parcel_df)
  final_without_geom <- sf::st_drop_geometry(final_df)
  final_df$has_false <- rowSums(final_without_geom == FALSE, na.rm = T)
  final_df$has_maybe <- rowSums(final_without_geom == "MAYBE", na.rm = T)
  # add the "allowed" and "reason" columns
  final_df <- final_df |>
    dplyr::mutate(allowed = ifelse(has_false > 0, FALSE, ifelse(has_maybe > 0, "MAYBE",TRUE)),
                  reason = ifelse(!is.na(maybe_reasons) | !is.na(false_reasons),
                                  paste("FALSE:", false_reasons, "- MAYBE:", maybe_reasons),
                                  "Building allowed")) |>
    dplyr::select(!c("has_false","has_maybe"))

  # select only the columns needed depending on whether detailed check is TRUE or FALSE
  if (detailed_check == FALSE){
    final_df <- final_df |>
      dplyr::select(dplyr::any_of(c("parcel_id",
                              "allowed",
                              "reason",
                              "geometry")))
  } else{
    final_df <- final_df |>
      dplyr::select(!dplyr::any_of(c("maybe_reasons",
                              "false_reasons",
                              "lot_width",
                              "lot_depth",
                              "lot_area",
                              "lot_type",
                              "zoning_id",
                              "pd_id",
                              "overlay_id")))
  }


  ## DEALING WITH DUPLICATE PARCEL_IDs ##
  # these are the few parcels that had two districts overlapping

  # get duplicate parcel_id names
  duplicates <- unique(final_df$parcel_id[duplicated(final_df$parcel_id)])

  if (length(duplicates) > 0){
    # loop through each duplicated parcel_id
    new_dfs <- list()
    length(new_dfs) <- length(duplicates)
    for (i in 1:length(duplicates)){
      id <- duplicates[[i]]

      # filter to just the first duplicate ids
      new_df <- final_df |>
        dplyr::filter(parcel_id == id)

      # make a vector of all the allowed values
      allowed_vals <- new_df$allowed

      # if all duplicates are TRUE, then it is still TRUE
      # if all duplicates are FALSE, then it is still FALSE
      # if any other combination, it is MABYE
      if (sum(allowed_vals == TRUE) == length(allowed_vals)){
        val <- TRUE
      } else if (sum(allowed_vals == FALSE) == length(allowed_vals)){
        val <- FALSE
      } else{
        val <- "MAYBE"
      }

      # this just groups the rows so I can combine the reason
      updated <- new_df |>
        dplyr::group_by(parcel_id) |>
        dplyr::summarise(allowed = val,
                         reason = paste(reason,collapse = " ---||--- "))

      new_reason <- updated$reason

      # make a new df with just one row for the parcel_id
      updated_df <- new_df[1,]
      updated_df[1,"allowed"] <- as.character(val)
      updated_df[1,"reason"] <- new_reason
      # add that df to a list of the combined parcel_id dfs
      new_dfs[[i]] <- updated_df

    }

    # make one df out of all the combined parcel_id dfs
    combined_duplicates <- dplyr::bind_rows(new_dfs)

    # take out the old duplicated parcel_id rows
    final_df <- final_df |>
      dplyr::filter(!parcel_id %in% duplicates)

    # add the new combined parcel_id rows
    final_df <- rbind(final_df, combined_duplicates)
  }


  ## RUN STATISTICS ##
  # report total runtime and other statistics
  total_time <- proc.time()[[3]] - total_start_time
  if (print_checkpoints){
    cat("_____summary_____\n")
    cat(paste0("total runtime: ", round(total_time,1), " sec (",round(total_time / 60,2)," min)\n"))
    cat(paste(length(which(final_df$allowed == TRUE)), "/", nrow(final_df), "parcels allow the building\n"))
    if (length(which(final_df$allowed == "MAYBE")) > 0){
      cat(paste(length(which(final_df$allowed == "MAYBE")), "/", nrow(final_df), "parcels might allow the building\n\n\n"))
    }
  } else{
    cat("zoning checks finished\n")
    cat(cat(paste0("total runtime: ", round(total_time,1), " sec (",round(total_time / 60,2)," min)\n\n\n")))
  }


  # Return the final data frame
  # It will contain every parcel with an "allowed" column and a "reason" column
  return(final_df)

}

final_df |>
  ggplot() +
  geom_sf(aes(color = check_pd))


bldg_file <- "../personal_rpoj/tidyzoning2.0/tidybuildings/tiny_tests/tiny_test2.bldg"
parcels_file <- "../personal_rpoj/1_nza_to_ozfs/nza_to_ozfs/test_parcels/Addison.parcel"
zoning_file <-  "../personal_rpoj/1_nza_to_ozfs/nza_to_ozfs/ozfs_edited/Addison.zoning"
detailed_check <- TRUE
print_checkpoints <- TRUE
checks <- possible_checks
