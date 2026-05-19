################################
  ######## I think I want to put the overlay stuff here; 2026: here's the chunk to move earlier. do the math here.

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
  overlay_maybes <- overlay_df|>

    # group group duplicate parcels and their results
    dplyr::group_by(parcel_id) |>
    dplyr::summarise(allowed = allowed[[1]], comb_check = paste(allowed_now, collapse = " - ")) |>

    # logic to decide what the overall result would be
    dplyr::mutate(allowed_now = dplyr::case_when(
      grepl("FALSE",comb_check) ~ "FALSE",
      grepl("MAYBE",comb_check) ~ "MAYBE",
      TRUE ~ "TRUE")) |>

    dplyr::filter(allowed == FALSE & allowed_now == "MAYBE")

  # check their footprint against whole parcel
  if (nrow(overlay_maybes) > 0){
    overlay_foot_start_time <- proc.time()[[3]]
    error_parcels <- c()
    for (z in 1:nrow(overlay_maybes)){
      parcel_data <- overlay_maybes[z,]
      parcel_name <- parcel_data$parcel_id
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

        parcel_with_setbacks <- parcel_sides
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
      cat(paste(
        length(
          which(
            overlay_maybes[,"allowed_now"][[1]] %in% c(TRUE, 'MAYBE')
            )
          )
        ,"parcels are TRUE or MAYBE\n\n"))
    }

  }

  #### change results based on overlay status ####

  # get a df of just the overlays
  overlay_check_df <- overlay_df |>
    sf::st_drop_geometry() |>
    dplyr::filter(has_overlay == "TRUE") |>

    # group group duplicate parcels and their results
    dplyr::group_by(parcel_id) |>
    dplyr::summarise(comb_check = paste(allowed_now, collapse = " - ")) |>

    # logic to decide what the overall result would be
    dplyr::mutate(overlay_check = dplyr::case_when(
      grepl("FALSE",comb_check) ~ "FALSE",
      grepl("MAYBE",comb_check) ~ "MAYBE",
      TRUE ~ "TRUE")) |>

    #select only necessary columns
    dplyr::select(parcel_id, overlay_check)

  if (nrow(overlay_check_df) > 0){

    # get a df of just the overlays that were retested for fit
    if (nrow(overlay_maybes) > 0){
      overlay_bldg_fit <- overlay_maybes |>
        sf::st_drop_geometry() |>

        # group group duplicate parcels and their results
        dplyr::group_by(parcel_id) |>
        dplyr::summarise(comb_check = paste(bldg_fit, collapse = " - ")) |>

        # logic to decide what the overall result would be
        dplyr::mutate(overlay_bldg_fit = dplyr::case_when(
          grepl("FALSE",comb_check) ~ "FALSE",
          grepl("MAYBE",comb_check) ~ "MAYBE",
          TRUE ~ "TRUE")) |>

        dplyr::select(parcel_id, overlay_bldg_fit)

    } else{
      overlay_bldg_fit <- data.frame(parcel_id = "id", overlay_bldg_fit = "maybe") |>
        dplyr::filter(parcel_id < 0)
    }

    new_final_df <- final_df |>
      # add the overlay_check column
      dplyr::left_join(overlay_check_df,
                       by = dplyr::join_by("parcel_id"),
                       relationship = "many-to-one") |>
      # add the overlay_bldg_fit column
      dplyr::left_join(overlay_bldg_fit,
                       by = dplyr::join_by("parcel_id"),
                       relationship = "many-to-one") |>
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
