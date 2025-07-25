#' Find where a building is allowed to be built
#'
#' `zr_run_zoning_checks()` checks the building information against all the
#' zoning constraints to see which parcels will allow the building.
#'
#' @param bldg_file The path to the OZFS *.bldg
#' @param parcel_files The path, or list of paths, to the OZFS *.parcel file or files
#' @param zoning_files The path, or list of paths, to the OZFS *.zoning file or files
#' @param detailed_check When TRUE, every parcel passes through each
#' check no matter the result, and it takes more time. When FALSE,
#' subsequent checks are skipped as soon as one check reads FALSE
#' @param print_checkpoints When TRUE, runtimes and other info will be
#' printed at certain points throughout the function.
#' @param checks A list of all the checks that should take place. The default is
#' to check for every constraint possible in the OZFS. These constraints can found
#' in the package data `possible_checks`. Note, if a zoning file doesn't have zoning
#' info for one of the constraints listed in the checks variable, then it is
#' assumed that building characteristic is allowed.
#' @param save_to The path or folder directory where you want to save the results
#' as a geojson file.
#'
#' @returns A simple features data frame with geometry for the centroid of each
#' parcel and columns to show info on allowance of the building.
#' @export
#'
#' @examples
zr_run_zoning_checks <- function(bldg_file,
                                 parcel_files,
                                 zoning_files,
                                 detailed_check = FALSE,
                                 print_checkpoints = TRUE,
                                 checks = possible_checks,
                                 save_to = NULL){

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
  zoning_data_list <- list()
  zoning_sf_list <- list()
  for (zoning_files_num in 1:length(zoning_files)){
    file <- zoning_files[[zoning_files_num]]
    zone_sf <- sf::st_read(file, quiet = TRUE) |>
      dplyr::filter(!sf::st_is_empty(geometry)) |>
      dplyr::mutate(muni_id = zoning_files_num)

    zone_sf$res_types_allowed <- zone_sf$res_types_allowed |> as.list()

    zoning_sf_list[[zoning_files_num]] <- zone_sf

    # zoning_data is the json list form of the zoning file
    # we will match the zoning_data_list idx with the zoning$muni_id column when we are
    # looping through zr_get_variables()
    zoning_data_list[[zoning_files_num]] <- rjson::fromJSON(file = file)
  }

  zoning_all_sf <- dplyr::bind_rows(zoning_sf_list)

  # get just the overlay districts with geometry
  overlays <- zoning_all_sf |>
    dplyr::filter(overlay == TRUE)
  # get just the pd_districts with geometry
  pd_districts <- zoning_all_sf |>
    dplyr::filter(planned_dev == TRUE)
  # get just the base districts with geometry
  # this is the one I will use for most of the checks
  zoning_sf <- zoning_all_sf |>
    dplyr::filter(overlay == FALSE) |>
    dplyr::filter(planned_dev == FALSE)

  # get appropriate crs in meters to use in the check footprint function
  crs <- zr_get_crs(zoning_sf)

  ## PARCELS ##
  # separate the parcel data into two special feature data frames
  parcels_sf_list <- list()
  for (parcel_file_num in 1:length(parcel_files)){
    file <- parcel_files[parcel_file_num]
    parcels_sf_list[[parcel_file_num]] <- sf::st_read(file, quiet = TRUE)
  }

  combined_parcel_files <- dplyr::bind_rows(parcels_sf_list)

  parcel_geo <- zr_get_parcel_geo(combined_parcel_files) # parcels with side labels
  parcel_dims <- zr_get_parcel_dims(combined_parcel_files) # parcels with centroid and dimensions

  # get unique parcel names to later find
  # which parcels don't have a zoning district covering them
  parcel_ids <- unique(parcel_dims$parcel_id)

  ## GET DISTRICT INDICES ##
  # use the base zoning districts to add zoning_id to parcel_dims
  parcel_dims <- zr_find_district_idx(parcel_dims, zoning_sf, "zoning_id")

  # use the pd districts to add zoning_id to parcel_dims
  pd_parcel_df <- zr_find_district_idx(parcel_dims, pd_districts, "pd_id")

  # use the overlay districts to add zoning_id to parcel_dims
  parcels_overlays <- zr_find_district_idx(parcel_dims, overlays, "overlay_id")

  # find which parcels don't have a zoning district covering them
  parcels_not_covered <- parcel_dims$parcel_id[is.na(parcel_dims$zoning_id)]
  pd_parcels_covered <- pd_parcel_df$parcel_id[!is.na(pd_parcel_df$pd_id)]
  overlay_parcels_covered <- parcels_overlays$parcel_id[!is.na(parcels_overlays$overlay_id)]
  parcels_not_covered <- parcels_not_covered[!parcels_not_covered %in% pd_parcels_covered]
  parcels_not_covered <- parcels_not_covered[!parcels_not_covered %in% overlay_parcels_covered]

  if (length(parcels_not_covered) > 0){
    warning(paste(length(parcels_not_covered),"/",nrow(parcel_dims),"parcels not covered by given zoning data"))
  }

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
    pd_parcel_df <- pd_parcel_df |>
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

  # DISTRICT CHECK
  parcel_df <- parcel_df |>
    dplyr::mutate(district_check = ifelse(is.na(zoning_id), "MAYBE", TRUE))

  if ("MAYBE" %in% unique(parcel_df$district_check)){
    parcel_df <- parcel_df |>
      dplyr::mutate(maybe_reasons = ifelse(parcel_id %in% pd_parcels,
                                           ifelse(!is.na(maybe_reasons),
                                                  paste(maybe_reasons, "no_district", sep = ", "),
                                                  "no_district"),
                                           maybe_reasons))

    # we filter the parcel_df to have just the TRUEs
    false_parcels <- parcel_df |>
      dplyr::filter(district_check == 'MAYBE')
    # Add the false_parcels to the false_df list
    false_df[[false_df_idx]] <- false_parcels
    false_df_idx <- false_df_idx + 1

    parcel_df <- parcel_df |>
      dplyr::filter(district_check == TRUE)
  }


  # GET ZONING REQUIREMENTS AND VARIABLES
  # this loop also creates a vector of parcels with not setback info to be used later
  zone_req_var_time <- proc.time()[[3]]

  vars_list <- list()
  zoning_req_list <- list()
  parcels_no_setbacks <- c()
  for (row_num in 1:nrow(parcel_df)){
    parcel_data <- parcel_df[row_num,]
    parcel_id <- as.character(parcel_data$parcel_id)
    district_data <- zoning_sf[parcel_data$zoning_id,]
    zoning_data <- zoning_data_list[[district_data$muni_id]]
    vars <- zr_get_variables(bldg_data, parcel_data, district_data, zoning_data)
    zoning_req <- zr_get_zoning_req(district_data, vars = vars)

    # check to see if it has setback and add it to the list
    if (inherits(zoning_req, "character")){
      parcels_no_setbacks <- c(parcels_no_setbacks, parcel_id)
    } else{
      setback_df <- zoning_req[grepl("setback",zoning_req$constraint_name),]
      if (sum(unlist(setback_df$min_value)) == 0 | is.na(sum(unlist(setback_df$min_value)))){
        parcels_no_setbacks <- c(parcels_no_setbacks, parcel_id)
      }
    }

    # add the data frames to a list
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


  # SIDE LABEL CHECK
  # if parcels have labeled sides or no setback requirements,
  # we can move on to the fit check
  if ("bldg_fit" %in% checks & nrow(parcel_df) > 0){
    parcels_known_sides <- unique(parcel_geo[parcel_geo$side != "unknown",]$parcel_id)
    parcels_for_bldg_fit <- unique(c(parcels_known_sides, parcels_no_setbacks))

    parcel_df <- parcel_df |>
      dplyr::mutate(parcel_side_lbl = ifelse(parcel_id %in% parcels_for_bldg_fit,TRUE, "MAYBE"),
                    maybe_reasons = ifelse(parcel_id %in% parcels_for_bldg_fit, maybe_reasons, ifelse(!is.na(maybe_reasons),paste(maybe_reasons, "side_lbl", sep = ", "),"side_lbl")))

    parcel_no_sides <- parcel_df |>
      dplyr::filter(!parcel_id %in% parcels_for_bldg_fit)

    false_df[[false_df_idx]] <- parcel_no_sides
    false_df_idx <- false_df_idx + 1

    parcel_df <- parcel_df |>
      dplyr::filter(parcel_id %in% parcels_for_bldg_fit)

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
    # so it will be small for the next checks
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
    # make the df with the overlay district indexes
    parcels_overlays <- parcels_overlays |>
      dplyr::filter(!is.na(overlay_id))

    overlay_parcels <- unique(parcels_overlays$parcel_id)

    parcel_df <- parcel_df |>
      dplyr::mutate(check_overlay = ifelse(parcel_id %in% overlay_parcels,"MAYBE", TRUE),
                    maybe_reasons = ifelse(parcel_id %in% overlay_parcels, ifelse(!is.na(maybe_reasons),paste(maybe_reasons, "in_overlay", sep = ", "),"in_overlay"), maybe_reasons))

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
  parcel_df <- parcel_df |>
    dplyr::mutate(maybe_reasons = ifelse(is.na(maybe_reasons), "", maybe_reasons),
           false_reasons = ifelse(is.na(false_reasons), "", false_reasons))
  final_df <- dplyr::bind_rows(false_df, parcel_df)
  final_without_geom <- sf::st_drop_geometry(final_df)
  final_df$has_false <- rowSums(final_without_geom == FALSE, na.rm = T)
  final_df$has_maybe <- rowSums(final_without_geom == "MAYBE", na.rm = T)
  # add the "allowed" and "reason" columns
  final_df <- final_df |>
    dplyr::mutate(allowed = ifelse(has_false > 0, FALSE, ifelse(has_maybe > 0, "MAYBE",TRUE)))
  final_df <- final_df |>
    dplyr::mutate(reason = ifelse(allowed == FALSE,
                                  false_reasons,
                                  ifelse(allowed == "MAYBE",
                                         maybe_reasons,
                                         "Building allowed"))) |>
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

  # SAVE THE FILE
  if (!is.null(save_to)){
    # does the file exist?
    if (file.exists(save_to)){ # the file exists
      # is it a directory or file?
      if (file.info(save_to)$isdir){ # a directory
        # make a new file name
        new_name <- Sys.time()
        new_name <- gsub(" ", "__", new_name)
        new_name <- gsub("[-:]", "_", new_name)
        new_name <- gsub("\\.", "_", new_name)
        new_file_name <- paste0("/zr_output_",new_name,".geojson")
        new_file_path <- paste0(save_to,"/zr_output_",new_file_name)
        sf::write_sf(final_df, new_file_path)
        cat(paste("output saved to",new_file_path, "\n"))
      } else{ # a file
        # delete the file and then write it
        file_removed <- file.remove(save_to)
        sf::write_sf(final_df, save_to)
        cat(paste("output saved to",save_to, "\n"))
      }
    } else{ # the file does not exist
      # does the directory exist?
      if (file.exists(dirname(save_to))){ # directory exists
        # save the file
        sf::write_sf(final_df, save_to)
        cat(paste("output saved to",save_to, "\n"))
      } else{ # directory doesn't exist
        # warning
        warning("save_to directory doesn't seem to exist")
      }
    }
  }


  # Return the final data frame
  # It will contain every parcel with an "allowed" column and a "reason" column
  return(final_df)

}


# final_df |>
#   ggplot() +
#   geom_sf(aes(color = allowed))
#
#
# bldg_file <- "../personal_rpoj/tidyzoning2.0/tidybuildings/tiny_tests/tiny_test2.bldg"
# parcel_files <- "../personal_rpoj/1_nza_to_ozfs/nza_to_ozfs/test_parcels/Azle.parcel"
# zoning_files <-  "../personal_rpoj/1_nza_to_ozfs/nza_to_ozfs/ozfs_edited/Azle.zoning"
#
# bldg_file <- "inst/extdata/2_fam.bldg"
# parcel_files <- "inst/extdata/Paradise.parcel"
# zoning_files <-  "inst/extdata/Paradise.zoning"
#
# parcel_files <- list.files("../personal_rpoj/1_nza_to_ozfs/nza_to_ozfs/test_parcels", full.names = TRUE)
# zoning_files <- list.files("../personal_rpoj/1_nza_to_ozfs/nza_to_ozfs/test_ozfs", full.names = TRUE)
#
#
# detailed_check <- TRUE
# print_checkpoints <- TRUE
# checks <- possible_checks
