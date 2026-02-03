#' Find where a building is allowed to be built
#'
#' `zr_run_zoning_checks()` checks the building information against all the
#' zoning constraints to see which parcels will allow the building.
#'
#' @section Zoning Checks:
#' The checks that can take place in `zr_run_zoning_checks()` are included in
#' the table below. Most checks are not required and only checked if specified
#' by the user in the `checks` argument. All optional checks possible are
#' stored in the `possible_checks` data which is the default value of the `checks`
#' argument. A few checks are built in to the function and, therefore, not a part of the
#' `checks` argument.
#'
#' The resulting data frame of `zr_run_zoning_checks()` will have a "reason" column with
#' the names of any checks that caused a FALSE or a MAYBE building allowance. For
#' more details on what the reasons column means, see the package data `reasons`.
#'
#'
#' @param bldg_file The path to the OZFS *.bldg
#' @param parcel_files The path to an OZFS `.parcel` file, or the path to a
#' folder containing OZFS `.parcel` files
#' @param zoning_files The path to an OZFS `.zoning` file, or the path to a
#' folder containing OZFS `.zoning` files
#' @param detailed_check When TRUE, every parcel passes through each
#' check no matter the result, and it takes more time. When FALSE,
#' subsequent checks are skipped as soon as one check reads FALSE
#' @param print_checkpoints When TRUE, runtimes and other info will be
#' printed at certain points throughout the function.
#' @param checks A list of all the checks that should take place. The default is
#' to check for every constraint possible in the OZFS. These constraints can be found
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
#' zoning_file <- zr_example_files("Paradise.zoning")
#' parcel_file <- zr_example_files("Paradise.parcel")
#' bldg_file <- zr_example_files("2_fam.bldg")
#'
#' # only checking against height constraints for speed
#' just_check_height <- zr_run_zoning_checks(bldg_file = bldg_file,
#'                                           parcel_files = parcel_file,
#'                                           zoning_files = zoning_file,
#'                                           checks = "height")
zr_run_zoning_checks <- function(bldg_file,
                                 parcel_files,
                                 zoning_files,
                                 detailed_check = FALSE,
                                 print_checkpoints = TRUE,
                                 checks = possible_checks,
                                 save_to = NULL){

  # track the start time to give a time stamp at end
  total_start_time <- proc.time()[[3]]

  if (length(zoning_files) > 1 & length(parcel_files) > 1){
    stop("Incorrect parcel_files and zoning_files input. Must be one file path, not multiple.")
  } else if (length(zoning_files) > 1){
    stop("Incorrect zoning_files input. Must be one file path, not multiple.")
  } else if (length(parcel_files) > 1){
    stop("Incorrect parcel_files input. Must be one file path, not multiple.")
  }

  # make sure input is correct and turn it into a list of zoning files
  if (file.exists(zoning_files)){ # is an existing file
    if (file.info(zoning_files)$isdir){ # is a folder
      zoning_files <- list.files(path = zoning_files, pattern = "\\.zoning$", full.names = TRUE)
      if (length(zoning_files) == 0){
        stop("No .zoning files found in given zoning_files directory")
      }
    } else{ # is a file
      if (grepl(pattern = "\\.zoning$", zoning_files)){ # is a .zoning file
        zoning_files <- zoning_files
      } else{ # is not a .zoning file
        stop("zoning_files is not a .zoning file")
      }
    }
  } else{ # is not an existing file
    stop("zoning_files input is not an existing file path")
  }

  # make sure input is correct and turn it into a list of parcel files
  if (file.exists(parcel_files)){ # is an existing file
    if (file.info(parcel_files)$isdir){ # is a folder
      parcel_files <- list.files(path = parcel_files, pattern = "\\.parcel$", full.names = TRUE)
      if (length(parcel_files) == 0){
        stop("No .parcel files found in given parcel_files directory")
      }
    } else{ # is a file
      if (grepl(pattern = "\\.parcel$", parcel_files)){ # is a .zoning file
        parcel_files <- parcel_files
      } else{ # is not a .zoning file
        stop("parcel_files is not a .parcel file")
      }
    }
  } else{ # is not an existing file
    stop("parcel_files input is not an existing file path")
  }

  incorrect_checks <- checks[!checks %in% possible_checks]

  if (length(incorrect_checks) > 0){
    warning(paste("Unknown constraints assigned to check input:",paste(incorrect_checks, collapse =", ")))
  }

  initial_checks <- checks[!checks %in% c("res_type",
                                       "unit_size",
                                       "bldg_fit",
                                       "overlay")]

  ########---- START DATA PREP----########
  ## the building json list ##
  bldg_data <- rjson::fromJSON(file = bldg_file)

  ## zoning data ##
  # get the full ozfs data as an sf data frame
  zoning_data_list <- list()
  zoning_sf_list <- list()
  for (zoning_files_num in 1:length(zoning_files)){
    file <- zoning_files[[zoning_files_num]]

    # zoning_data is the json list form of the zoning file
    # we will match the zoning_data_list idx with the zoning$muni_id column when we are
    # looping through zr_get_variables()
    zoning_data_from_json <- rjson::fromJSON(file = file)
    zoning_data_list[[zoning_files_num]] <- zoning_data_from_json
    zoning_data_muni_name <- zoning_data_from_json$muni_name

    # get rid of empty geometries and add a muni_id and muni_name to the zoning df

    suppressWarnings(
      zone_sf <- sf::st_read(file, quiet = TRUE) |>
        dplyr::filter(!sf::st_is_empty(geometry)) |>
        dplyr::mutate(muni_id = zoning_files_num,
                      muni_name = zoning_data_muni_name)
    )

    # make res_types_allowed a list so that it will bind properly
    zone_sf$res_types_allowed <- zone_sf$res_types_allowed |> as.list()

    if (is.null(zone_sf$overlay)){
      zone_sf$overlay <- "FALSE"
    } else{
      zone_sf$overlay[is.na(zone_sf$overlay)] <- "FALSE"
    }

    if (is.null(zone_sf$planned_dev)){
      zone_sf$planned_dev <- FALSE
    } else{
      zone_sf$planned_dev[is.na(zone_sf$planned_dev)] <- FALSE
    }

    # add the zoning df to a list that will later be bound into one df
    zoning_sf_list[[zoning_files_num]] <- zone_sf
  }

  zoning_all_sf <- dplyr::bind_rows(zoning_sf_list)

  # get just the overlay districts with geometry
  overlays <- zoning_all_sf |>
    dplyr::filter(overlay != FALSE)
  # get just the pd_districts with geometry
  pd_districts <- zoning_all_sf |>
    dplyr::filter(planned_dev != FALSE)
  # get just the base districts with geometry note some base districts will be PD districts
  # this is the one I will use for most of the checks
  zoning_sf <- zoning_all_sf |>
    dplyr::filter(overlay == FALSE)


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

  # check for parcel duplicates and update as necessary
  dups <- which(duplicated(parcel_dims$parcel_id) | duplicated(parcel_dims$parcel_id, fromLast = TRUE))
  dups_deleted <- 0
  dups_edited <- 0
  for (id in unique(parcel_dims$parcel_id[dups])) {
    idx <- which(parcel_dims$parcel_id == id)
    geom_text <- sf::st_as_text(parcel_dims$geometry[idx])
    if (all(geom_text[[1]] == geom_text)) {
      # If all geometries are equal, keep the first, drop others
      parcel_dims <- parcel_dims[-idx[-1], ]
      dups_deleted <- dups_deleted + 1
    } else {
      # If geometries differ, assign new IDs to duplicates beyond the first
      for (j in 2:length(idx)) {
        parcel_dims$parcel_id[idx[j]] <- paste0(parcel_dims$parcel_id[idx[j]], "_", j)
        dups_edited <- dups_edited + 1
      }
    }
  }

  # give a warning explaining what was done to the duplicates
  if (dups_deleted > 0 & dups_edited > 0){
    warning(paste0(dups_deleted, " parcels removed based on duplicate geometry and parcel_id \n",
                  dups_edited, " parcel ids edited based on duplicate parcel_id but different geometry"))
  } else if (dups_deleted > 0){
    warning(paste0(dups_deleted, " parcels removed based on duplicate geometry and parcel_id"))
  } else if (dups_edited > 0){
    warning(paste0(dups_edited, " parcel ids edited based on duplicate parcel_id but different geometry"))
  }


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

  zoning_is_na <- parcel_dims$zoning_id |>
    unique() |>
    is.na()

  if (length(zoning_is_na) == 1 & TRUE %in% zoning_is_na){
    stop("Zoning districts and parcels do not appear to overlap.")
  }

  # add false_reasons and maybe_reasons columns to parcel_dims (for tracking maybes and falses)
  # filter it to only the parcels that have a base district
  # add the muni_name and dist_abbr
  # this parcel_df is what we will use for most of the calculations
  dist_abbr_vec <- zoning_sf$dist_abbr
  muni_name_vec <- zoning_sf$muni_name

  parcel_df <- parcel_dims |>
    dplyr::mutate(false_reasons = as.character(NA),
                  maybe_reasons = as.character(NA))

  parcel_df$muni_name <- muni_name_vec[parcel_df$zoning_id]
  parcel_df$dist_abbr <- dist_abbr_vec[parcel_df$zoning_id]


  # GETTING RID OF NA PARCELS #
  # find which parcels don't have a zoning district covering them
  parcels_not_covered <- parcel_df$parcel_id[is.na(parcel_df$zoning_id)]

  if (length(parcels_not_covered) > 0){
    warning(paste(length(parcels_not_covered),"/",nrow(parcel_df),"parcels not covered by a base district. Excluded from analysis"))
  }

  # get rid of parcels that don't have a base district
  parcel_df <- parcel_df |>
    dplyr::filter(!is.na(zoning_id))

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

    pd_overlay_idx <- which(pd_districts$overlay == TRUE)

    # make a new df with the pd district indexes
    pd_parcel_df <- pd_parcel_df |>
      dplyr::filter(!is.na(pd_id))

    # add muni_name and dist_abbr to the df
    dist_abbr_vec_pd <- pd_districts$dist_abbr
    muni_name_vec_pd <- pd_districts$muni_name

    pd_parcel_df$muni_name <- muni_name_vec_pd[pd_parcel_df$pd_id]
    pd_parcel_df$dist_abbr <- dist_abbr_vec_pd[pd_parcel_df$pd_id]


    # get the unique parcel names that are in pd districts
    pd_parcels <- unique(pd_parcel_df$parcel_id)

    # get the unique pd district abbreviations
    pd_dist_abbr <- unique(pd_parcel_df$dist_abbr)

    # get the parcel names of the ones in pd_overlays
    pd_parcel_overlay_df <- pd_parcel_df[pd_parcel_df$pd_id %in% pd_overlay_idx,]
    pd_parcels_overlay <- unique(pd_parcel_overlay_df$parcel_id)

    if (length(pd_overlay_idx) > 0){
      parcel_df <- parcel_df |>
        dplyr::mutate(check_pd = ifelse((parcel_id %in% pd_parcels & dist_abbr %in% pd_dist_abbr) | parcel_id %in% pd_parcels_overlay, FALSE, TRUE),
                      false_reasons = ifelse(parcel_id %in% pd_parcels_overlay, ifelse(!is.na(false_reasons),paste(false_reasons, "PD_overlay", sep = ", "),"PD_overlay"),
                                             ifelse(parcel_id %in% pd_parcels & dist_abbr %in% pd_dist_abbr, ifelse(!is.na(false_reasons),paste(false_reasons, "PD_dist", sep = ", "),"PD_dist"), false_reasons)))
    } else{
      parcel_df <- parcel_df |>
        dplyr::mutate(check_pd = ifelse((parcel_id %in% pd_parcels & dist_abbr %in% pd_dist_abbr) | parcel_id %in% pd_parcels_overlay, FALSE, TRUE),
                      false_reasons = ifelse(parcel_id %in% pd_parcels & dist_abbr %in% pd_dist_abbr, ifelse(!is.na(false_reasons),paste(false_reasons, "PD_dist", sep = ", "),"PD_dist"), false_reasons))
    }

    # if detailed_check == FALSE, then we store the FALSE parcels in a list to be combined later
    # we filter the parcel_df to have just the TRUEs and MAYBEs
    # so it will be small for the next checks
    if (detailed_check == FALSE){
      false_parcels <- parcel_df |>
        dplyr::filter(check_pd == FALSE)
      # Add the false_parcels to the false_df list
      false_df[["pd_check"]] <- false_parcels

      parcel_df <- parcel_df |>
        dplyr::filter(check_pd == TRUE)
    }

    # print checkpoint info
    if (print_checkpoints){
      time_lapsed <- proc.time()[[3]] - pd_time
      cat(ifelse(time_lapsed > 60,
                 paste0("___planned_dev_check___(",round(time_lapsed / 60,2), " min)\n"),
                 paste0("___planned_dev_check___(",round(time_lapsed,1), " sec)\n")))
      cat(paste(length(pd_parcels),"parcels in planned developement district\n\n"))
    }

  }


  # GET ZONING REQUIREMENTS AND VARIABLES
  # this loop also creates a vector of parcels with no setback info to be used later
  zone_req_var_time <- proc.time()[[3]]

  vars_list <- list()
  zoning_req_list <- list()
  parcels_no_setbacks <- c()

  # in the rare case that parcel_df has no rows, that means that
  # no parcels made it past the pd district check.
  if (nrow(parcel_df) == 0){
    stop("No parcel made it past the Planned Development check. Either all parcels are in a planned development, or the parcels and districts don't align.")
  }

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

    #checking unit_size
    if ("unit_size" %in% checks){
      unit_check_df <- data.frame(unit_size = as.character(zr_check_unit(district_data, bldg_data, vars)))
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
    false_df[["initial_check"]] <- false_parcels
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

    false_df[["side_label_check"]] <- parcel_no_sides

    parcel_df <- parcel_df |>
      dplyr::filter(parcel_id %in% parcels_for_bldg_fit)

  }


  # FIT CHECK
  # see if the building footprint fits in the parcel's buildable area

  if ("bldg_fit" %in% checks & nrow(parcel_df) > 0 & !is.null(parcel_geo)){
    foot_start_time <- proc.time()[[3]]
    error_parcels <- c()
    for (z in 1:nrow(parcel_df)){
      parcel_data <- parcel_df[z,]
      parcel_name <- parcel_data$parcel_id
      district_data <- zoning_sf[parcel_data$zoning_id,]
      zoning_req <- zoning_req_list[[parcel_data$parcel_id]]
      vars <- vars_list[[parcel_data$parcel_id]]

      # if the footprint area is smaller than the parcel area,
      # then run the check_fit function
      if (vars$lot_cov_bldg <= 100){
        parcel_sides <- tryCatch(
          {
            parcel_geo |>
              dplyr::filter(parcel_id == parcel_data$parcel_id)
          }, error = function(e) {
            error <- "error"
            class(error) <- "error"
            return(error)
          }
        )

        if (inherits(parcel_sides, "error")){
          error_parcels <- c(error_parcels, parcel_name)
        }

        parcel_with_setbacks <- tryCatch(
          {
            zr_add_setbacks(parcel_sides, district_data, zoning_req)
          }, error = function(e) {
            error <- "error"
            class(error) <- "error"
            return(error)
          }
        )

        if (inherits(parcel_with_setbacks, "error")){
          error_parcels <- c(error_parcels, parcel_name)
        }

        buildable_area <- tryCatch(
          {
            zr_get_buildable_area(parcel_with_setbacks, crs)
          }, error = function(e) {
            error <- "error"
            class(error) <- "error"
            return(error)
          }
        )

        if (inherits(buildable_area, "error")){
          error_parcels <- c(error_parcels, parcel_name)
        }

        # parcel_sides <- parcel_geo |>
        #   dplyr::filter(parcel_id == parcel_data$parcel_id)
        # parcel_with_setbacks <- zr_add_setbacks(parcel_sides, district_data, zoning_req)
        # buildable_area <- zr_get_buildable_area(parcel_with_setbacks, crs)

        # if two buildable areas were recorded, we need to test for both
        check_1_maybe <- FALSE
        if (length(buildable_area) > 1){

          check_1 <- tryCatch(
            {
              zr_check_fit(bldg_data, sf::st_make_valid(buildable_area[[1]]), crs = crs)
            }, error = function(e) {
              error <- "error"
              class(error) <- "error"
              return(error)
            }
          )

          if (inherits(check_1, "error")){
            error_parcels <- c(error_parcels, parcel_name)
            check_1 <- TRUE
            check_1_maybe <- TRUE
          }

          # check_1 <- zr_check_fit(bldg_data, sf::st_make_valid(buildable_area[[1]]), crs = crs)

          if (check_1 & check_1_maybe){
            check <- "MAYBE"
          } else if (check_1){
            check <- check_1
          } else{

            check_2 <- tryCatch(
              {
                zr_check_fit(bldg_data, sf::st_make_valid(buildable_area[[2]]), crs = crs)
              }, error = function(e) {
                error <- "error"
                class(error) <- "error"
                return(error)
              }
            )

            if (inherits(check_2, "error")){
              error_parcels <- c(error_parcels, parcel_name)
              check_2 <- TRUE
            }

            # check_2 <- zr_check_fit(bldg_data, sf::st_make_valid(buildable_area[[2]]), crs = crs)
            if (check_2){
              check <- "MAYBE"
            } else{
              check <- FALSE
            }
          }

        } else{
          check <- tryCatch(
            {
              zr_check_fit(bldg_data, sf::st_make_valid(buildable_area[[1]]), crs = crs)
            }, error = function(e) {
              error <- "error"
              class(error) <- "error"
              return(error)
            }
          )

          if (inherits(check, "error")){
            error_parcels <- c(error_parcels, parcel_name)
            check <- "MAYBE"
          }

          # check <- zr_check_fit(bldg_data, sf::st_make_valid(buildable_area[[1]]), crs = crs)
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

    if (length(error_parcels > 0)){
      warning(paste0("The following parcels were marked as MAYBE because they produced errors during zr_check_fit:\n", paste(unique(error_parcels),collapse = "\n")))
    }

    # if detailed_check == FALSE, then we store the FALSE parcels in a list to be combined later
    # we filter the parcel_df to have just the TRUEs and MAYBEs
    # so it will be small for the next checks
    if (detailed_check == FALSE){
      false_parcels <- parcel_df[parcel_df[,"bldg_fit"][[1]] == FALSE,]
      parcel_df <- parcel_df[parcel_df[,"bldg_fit"][[1]] %in% c(TRUE, "MAYBE"),]
      # Add the false_parcels to the false_df list
      false_df[["fit_check"]] <- false_parcels
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

  ########----FINALIZING THINGS----########
  # combined all the false_df and the parcel_df
  parcel_df <- parcel_df |>
    dplyr::mutate(maybe_reasons = ifelse(is.na(maybe_reasons), "", maybe_reasons),
           false_reasons = ifelse(is.na(false_reasons), "", false_reasons))
  class(parcel_df$false_reasons) <- "character"
  class(parcel_df$maybe_reasons) <- "character"
  if (nrow(parcel_df) > 0){
    final_df <- dplyr::bind_rows(false_df) |>
      dplyr::bind_rows(parcel_df)
  } else{
    final_df <- dplyr::bind_rows(false_df)
  }
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


  ################################
  ######## I think I want to put the overlay stuff here

  parcels_overlays <- parcels_overlays |>
    dplyr::filter(!is.na(overlay_id))

  overlay_parcels <- unique(parcels_overlays$parcel_id)

  parcels_overlays$overlay_type <- overlays$overlay[parcels_overlays$overlay_id]

  overlay_types <- parcels_overlays |>
    dplyr::select(parcel_id, overlay_type) |>
    sf::st_drop_geometry()

  overlay_df <- final_df |>
    # just select relevent columns
    dplyr::select(parcel_id, false_reasons, allowed) |>
    # filter out the ones that are already FALSE because they are in PD districts
    dplyr::filter(grepl("PD_dist",false_reasons) == FALSE) |>
    dplyr::filter(grepl("PD_overlay",false_reasons) == FALSE) |>
    # add a column to label the ones that are covered by an overlay
    dplyr::mutate(has_overlay = parcel_id %in% overlay_parcels) |>
    # join overlay_types df to list the type of overlay stated in the OZFS
    dplyr::left_join(overlay_types, by = dplyr::join_by(parcel_id)) |>
    # do some logic to see if the result changes based on overly type
    dplyr::mutate(allowed_now = dplyr::case_when(
      # if ambiguous
      allowed == "MAYBE" ~ "MAYBE",
      # if building meets base requirements
      allowed == "TRUE" & is.na(overlay_type) ~ "TRUE",
      allowed == "TRUE" & overlay_type == "no-residentail-effect" ~ "TRUE",
      allowed == "TRUE" & overlay_type == "replace" ~ "MAYBE",
      allowed == "TRUE" & overlay_type == "relax" ~ "TRUE",
      allowed == "TRUE" & overlay_type == "restrict" ~ "MAYBE",
      allowed == "TRUE" & overlay_type == "demolition-only" ~ "TRUE",
      # if building doesn't meet base requirements
      allowed == "FALSE" & is.na(overlay_type) ~ "FALSE",
      allowed == "FALSE" & overlay_type == "no-residentail-effect" ~ "FALSE",
      allowed == "FALSE" & overlay_type == "replace" ~ "MAYBE", # check fit on parcel
      allowed == "FALSE" & overlay_type == "relax" ~ "MAYBE", # check fit on parcel
      allowed == "FALSE" & overlay_type == "restrict" ~ "FALSE",
      allowed == "FALSE" & overlay_type == "demolition-only" ~ "FALSE",
      TRUE ~ "FALSE"
    ))


  # get the rows that changed from FALSE to MAYBE with overlays
  # we will have to recheck to see if the building fits in the parcel without setbacks
  overlay_maybes <- overlay_df |>
    dplyr::filter(allowed == FALSE & allowed_now == "MAYBE")

  # check their footprint against whole parcel
  if (nrow(overlay_maybes) > 0){
    overlay_foot_start_time <- proc.time()[[3]]
    error_parcels <- c()
    for (z in 1:nrow(overlay_maybes)){
      parcel_data <- overlay_maybes[z,]
      parcel_name <- parcel_data$parcel_id
      district_data <- zoning_sf[parcel_data$zoning_id,]
      zoning_req <- zoning_req_list[[parcel_data$parcel_id]]
      vars <- vars_list[[parcel_data$parcel_id]]

      # if the footprint area is smaller than the parcel area,
      # then run the check_fit function
      if (vars$lot_cov_bldg <= 100){
        parcel_sides <- tryCatch(
          {
            parcel_geo |>
              dplyr::filter(parcel_id == parcel_data$parcel_id)
          }, error = function(e) {
            error <- "error"
            class(error) <- "error"
            return(error)
          }
        )

        if (inherits(parcel_sides, "error")){
          error_parcels <- c(error_parcels, parcel_name)
        }

        parcel_with_setbacks <- tryCatch(
          {
            zr_add_setbacks(parcel_sides, district_data, zoning_req)
          }, error = function(e) {
            error <- "error"
            class(error) <- "error"
            return(error)
          }
        )

        if (inherits(parcel_with_setbacks, "error")){
          error_parcels <- c(error_parcels, parcel_name)
        }

        parcel_with_setbacks$setback <- NA

        buildable_area <- tryCatch(
          {
            zr_get_buildable_area(parcel_with_setbacks, crs)
          }, error = function(e) {
            error <- "error"
            class(error) <- "error"
            return(error)
          }
        )

        if (inherits(buildable_area, "error")){
          error_parcels <- c(error_parcels, parcel_name)
        }

        check <- tryCatch(
          {
            zr_check_fit(bldg_data, sf::st_make_valid(buildable_area[[1]]), crs = crs)
          }, error = function(e) {
            error <- "error"
            class(error) <- "error"
            return(error)
          }
        )

        if (inherits(check, "error")){
          error_parcels <- c(error_parcels, parcel_name)
          check <- "MAYBE"
        }

      } else{
        check <- FALSE
      }

      overlay_maybes[z, "bldg_fit"] <- as.character(check)

    }

    if (length(error_parcels > 0)){
      warning(paste0("The following parcels were marked as MAYBE because they produced errors during zr_check_fit:\n", paste(unique(error_parcels),collapse = "\n")))
    }

    # print checkpoint info
    if (print_checkpoints){
      time_lapsed <- proc.time()[[3]] - overlay_foot_start_time
      cat(ifelse(time_lapsed > 60,
                 paste0("___bldg_fit_overlays___(",round(time_lapsed / 60,2), " min)\n"),
                 paste0("___bldg_fit_overlays___(",round(time_lapsed,1), " sec)\n")))
      cat(paste(length(which(overlay_maybes[,"bldg_fit_overlay"][[1]] %in% c(TRUE, 'MAYBE'))),"parcels are TRUE or MAYBE\n\n"))
    }

  }


  #### change results based on overlay status ####

  # get a df of just the overlays
  overlay_check_df <- overlay_df |>
    sf::st_drop_geometry() |>
    dplyr::filter(has_overlay == "TRUE") |>
    dplyr::select(parcel_id, overlay_check = allowed_now)

  if (nrow(overlay_check_df) > 0){

    # get a df of just the overlays that were retested for fit
    if (nrow(overlay_maybes) > 0){
      overlay_bldg_fit <- overlay_maybes |>
        sf::st_drop_geometry() |>
        dplyr::select(parcel_id, overlay_bldg_fit = bldg_fit)

    } else{
      overlay_bldg_fit <- data.frame(parcel_id = "id", overlay_bldg_fit = "maybe") |>
        dplyr::filter(parcel_id < 0)
    }

    new_final_df <- final_df |>
      # add the overlay_check column
      dplyr::left_join(overlay_check_df, by = dplyr::join_by("parcel_id")) |>
      # add the overlay_bldg_fit column
      dplyr::left_join(overlay_bldg_fit, by = dplyr::join_by("parcel_id")) |>
      # create an overlay column to give the results of the overlay check
      dplyr::mutate(overlay = dplyr::case_when(
        is.na(overlay_bldg_fit) ~ overlay_check,
        overlay_bldg_fit == "FALSE" ~ "FALSE",
        overlay_bldg_fit == "TRUE" ~ "MAYBE",
        TRUE ~ "TRUE"
      )) |>
      # update the reason column in case there was an overlay that changed it
      dplyr::mutate(reason = dplyr::case_when(
        is.na(overlay_bldg_fit) ~ reason,
        overlay_bldg_fit == "TRUE" ~ ifelse(is.na(reason), "bldg_fit_overlay", paste(reason, "bldg_fit_overlay", sep = ", ")),
        overlay_bldg_fit == "FALSE" ~ ifelse(is.na(reason), "overlay", paste(reason, "overlay", sep = ", "))
      )) |>
      # update the false_reasons column in case there is an overlay reason to add
      dplyr::mutate(false_reasons = dplyr::case_when(
        is.na(overlay_bldg_fit) ~ false_reasons,
        overlay_bldg_fit == "FALSE" ~ ifelse(is.na(false_reasons), "overlay", paste(false_reasons, "overlay", sep = ", ")),
        TRUE ~ false_reasons
      )) |>
      # update the maybe_reasons column in case there is an overlay reason to add
      dplyr::mutate(maybe_reasons = dplyr::case_when(
        is.na(overlay_bldg_fit) ~ maybe_reasons,
        overlay_bldg_fit == "TRUE" ~ ifelse(is.na(maybe_reasons), "bldg_fit_overlay", paste(maybe_reasons, "bldg_fit_overlay", sep = ", ")),
        TRUE ~ maybe_reasons
      )) |>
      # mutate the allowed column to update the values that were changed by overlay info
      dplyr::mutate(allowed = as.character(allowed)) |>
      dplyr::mutate(allowed = dplyr::case_when(
        is.na(overlay_bldg_fit) & is.na(overlay_check) ~ allowed,
        is.na(overlay_bldg_fit) ~ overlay_check,
        overlay_bldg_fit == "TRUE" ~ "MAYBE",
        overlay_bldg_fit == "FALSE" ~ "FALSE",
        TRUE ~ overlay_check
      ))

  } else{
    new_final_df <- final_df
  }



  # select only the columns needed depending on whether detailed check is TRUE or FALSE
  if (detailed_check == FALSE){
    final_result <- new_final_df |>
      dplyr::select(dplyr::any_of(c("parcel_id",
                                    "muni_name",
                                    "dist_abbr",
                                    "allowed",
                                    "reason",
                                    "geometry")))
  } else{
    final_result <- new_final_df |>
      dplyr::select(!dplyr::any_of(c("maybe_reasons",
                                     "false_reasons",
                                     "lot_width",
                                     "lot_depth",
                                     "lot_area",
                                     "lot_type",
                                     "zoning_id",
                                     "pd_id",
                                     "overlay_id",
                                     "overlay_check",
                                     "overlay_bldg_fit")))
  }


  ## DEALING WITH DUPLICATE PARCEL_IDs ##
  # these are the few parcels that had two districts overlapping

  # get duplicate parcel_id names
  duplicates <- unique(final_result$parcel_id[duplicated(final_result$parcel_id)])

  if (length(duplicates) > 0){

    warning(paste(length(duplicates),"parcels are covered by multiple base districts and are duplicated in results to account for each of those base districts. (see is_duplicate column)"))

    final_result$is_duplicate[duplicated(final_result$parcel_id) | duplicated(final_result$parcel_id, fromLast = TRUE)] <- TRUE
    final_result$is_duplicate[is.na(final_result$is_duplicate)] <- FALSE

  }


  ## RUN STATISTICS ##
  # report total runtime and other statistics
  total_time <- proc.time()[[3]] - total_start_time
  if (print_checkpoints){
    cat("_____summary_____\n")
    cat(paste0("total runtime: ", round(total_time,1), " sec (",round(total_time / 60,2)," min)\n"))
    cat(paste(length(which(final_result$allowed == TRUE)), "/", nrow(final_result), "parcels allow the building\n"))
    if (length(which(final_result$allowed == "MAYBE")) > 0){
      cat(paste(length(which(final_result$allowed == "MAYBE")), "/", nrow(final_result), "parcels might allow the building\n\n\n"))
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
        sf::write_sf(final_result, new_file_path)
        cat(paste("output saved to",new_file_path, "\n"))
      } else{ # a file
        # delete the file and then write it
        file_removed <- file.remove(save_to)
        sf::write_sf(final_result, save_to)
        cat(paste("output saved to",save_to, "\n"))
      }
    } else{ # the file does not exist
      # does the directory exist?
      if (file.exists(dirname(save_to))){ # directory exists
        # save the file
        sf::write_sf(final_result, save_to)
        cat(paste("output saved to",save_to, "\n"))
      } else{ # directory doesn't exist
        # warning
        warning("save_to directory doesn't seem to exist")
      }
    }
  }

  if (!is.null(final_result$lot_area.1)){
    names(final_result)[names(final_result) == "lot_area.1"] <- "lot_area"
  }

  # Return the final data frame
  # It will contain every parcel with an "allowed" column and a "reason" column
  return(final_result)

}
