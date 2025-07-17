#' Get all OZFS variables in one table
#'
#' @param bldg_data either the file path to an OZFS *.bldg file or
#' a list created from the the *.bldg file using `rjson::fromJSON`
#' @param parcel_data one row of a parcel data frame created from the
#' OZFS *.parcel file
#' @param district_data one row (representing one district) of a
#' zoning data frame created from the OZFS *.zoning file
#' @param zoning_data either the path to a *.zoning file or
#' a list created from the the *.zoning file using `rjson::fromJSON`
#'
#'
#' @returns a one-row data frame with a column for each variable
#' @export
#'
#' @examples
#'
zr_get_variables <- function(bldg_data, parcel_data, district_data, zoning_data){

  if (class(zoning_data)[[1]] == "character"){
    if (file.exists(zoning_data)){
      zoning_json <- tryCatch({
        rjson::fromJSON(file = zoning_data)
      }, error = function(e) {
        stop("The zoning_data file path does not seem to be in json/geojson format")
      })

      zoning_defs <- zoning_json$definitions
    }
  } else if (class(zoning_data)[[1]] == "list"){
    zoning_defs <- zoning_data$definitions
  } else{
    stop("Improper input: zoning_data")
  }

  if (class(bldg_data)[[1]] == "character"){
    bldg_json <- tryCatch({
      rjson::fromJSON(file = bldg_data)
    }, error = function(e) {
      stop("bldg_data must be a file path to an OZFS *.bldg file or a list created from said file")
    })
  } else if (class(zoning_data)[[1]] == "list"){
    bldg_json <- bldg_data
  } else{
    stop("Improper input: bldg_data")
  }



  if (is.null(bldg_json$bldg_info) | is.null(bldg_json$unit_info) | is.null(bldg_json$level_info)){
    stop("Improper format: json must contain bldg_info, unit_info, and level_info sections")
  }

  # creating a data frame for the unit info
  unit_info_df <- do.call(rbind.data.frame, bldg_json$unit_info)

  # creating a data frame for the level info
  level_info_df <- do.call(rbind.data.frame, bldg_json$level_info)

  # assigning the values to variables
  bldg_depth <- bldg_json$bldg_info$depth
  bldg_width <- bldg_json$bldg_info$width
  footprint <- bldg_width * bldg_depth
  dist_abbr <- district_data$dist_abbr
  fl_area <- sum(level_info_df$gross_fl_area)
  fl_area_first <- ifelse(length(level_info_df$gross_fl_area[level_info_df$level == 1]) == 1,
                          level_info_df$gross_fl_area[level_info_df$level == 1],
                          0)
  fl_area_top <- ifelse(sum(level_info_df$level > 1) > 0,
                        level_info_df$gross_fl_area[level_info_df$level == max(level_info_df$level)],
                        0)
  stories <- max(level_info_df$level)
  height_deck <- ifelse(!is.null(bldg_json$bldg_info$height_deck),bldg_json$bldg_info$height_deck,bldg_json$bldg_info$height_top)
  height_eave <- ifelse(!is.null(bldg_json$bldg_info$height_eave),bldg_json$bldg_info$height_eave,bldg_json$bldg_info$height_top)
  height_plate <- bldg_json$bldg_info$height_plate
  height_top <- bldg_json$bldg_info$height_top
  height_tower <- ifelse(!is.null(bldg_json$bldg_info$height_tower),bldg_json$bldg_info$height_tower,0)
  lot_area <- parcel_data$lot_area
  lot_depth <- parcel_data$lot_depth
  lot_type <- parcel_data$lot_area
  lot_width <- parcel_data$lot_width
  max_unit_size <- max(unit_info_df$fl_area)
  min_unit_size <- min(unit_info_df$fl_area)
  n_ground_entry <- sum(unit_info_df$qty[unit_info_df$entry_level == 1])
  n_outside_entry <- sum(unit_info_df$qty[unit_info_df$outside_entry == TRUE])
  parking_enclosed <- ifelse(!is.null(bldg_json$bldg_info$parking),bldg_json$bldg_info$parking,0)
  roof_type <- ifelse(!is.null(bldg_json$bldg_info$roof_type),bldg_json$bldg_info$roof_type,"flat")
  sep_platting <- ifelse(!is.null(bldg_json$bldg_info$sep_platting),bldg_json$bldg_info$sep_platting, FALSE)
  unit_separation <- ifelse(!is.null(bldg_json$bldg_info$unit_separation),bldg_json$bldg_info$unit_separation, "open_area")
  total_bedrooms <- sum(unit_info_df$bedrooms * unit_info_df$qty)
  total_units <- sum(unit_info_df$qty)
  units_0bed <- sum(unit_info_df$qty[unit_info_df$bedrooms == 0])
  units_1bed <- sum(unit_info_df$qty[unit_info_df$bedrooms == 1])
  units_2bed <- sum(unit_info_df$qty[unit_info_df$bedrooms == 2])
  units_3bed <- sum(unit_info_df$qty[unit_info_df$bedrooms == 3])
  units_4bed <- sum(unit_info_df$qty[unit_info_df$bedrooms > 3])
  unit_pct_0bed <- units_0bed / total_units
  unit_pct_1bed <- units_1bed / total_units
  unit_pct_2bed <- units_2bed / total_units
  unit_pct_3bed <- units_3bed / total_units
  unit_pct_4bed <- units_4bed / total_units
  unit_size_avg <- mean(unit_info_df$fl_area)
  lot_cov_bldg <- (footprint / (lot_area * 43560)) * 100
  unit_density <- total_units / lot_area
  far <- fl_area / (lot_area * 43560)
  # height: a variable that will be created at the end of the this function
  # res_type: a variable that will be created at the end of the this function
  # bedrooms: a variable that we will create and use only when we check unit size

  # making it a data frame to return
  vars_df <- data.frame(bldg_depth = bldg_depth,
                        bldg_width = bldg_width,
                        footprint = footprint,
                        dist_abbr = dist_abbr,
                        fl_area = fl_area,
                        fl_area_first = fl_area_first,
                        fl_area_top = fl_area_top,
                        stories = stories,
                        height_deck = height_deck,
                        height_eave = height_eave,
                        height_plate = height_plate,
                        height_top = height_top,
                        height_tower = height_tower,
                        lot_area = lot_area,
                        lot_depth = lot_depth,
                        lot_type = lot_type,
                        lot_width = lot_width,
                        max_unit_size = max_unit_size,
                        min_unit_size = min_unit_size,
                        n_ground_entry = n_ground_entry,
                        n_outside_entry = n_outside_entry,
                        parking_enclosed = parking_enclosed,
                        roof_type = roof_type,
                        sep_platting = sep_platting,
                        unit_separation = unit_separation,
                        total_bedrooms = total_bedrooms,
                        total_units = total_units,
                        units_0bed = units_0bed,
                        units_1bed = units_1bed,
                        units_2bed = units_2bed,
                        units_3bed = units_3bed,
                        units_4bed = units_4bed,
                        unit_pct_0bed = unit_pct_0bed,
                        unit_pct_1bed = unit_pct_1bed,
                        unit_pct_2bed = unit_pct_2bed,
                        unit_pct_3bed = unit_pct_3bed,
                        unit_pct_4bed = unit_pct_4bed,
                        unit_size_avg = unit_size_avg,
                        lot_cov_bldg = lot_cov_bldg,
                        unit_density = unit_density,
                        far = far)



  # the last variables we assign are the variables defined by the city zoning code
  # because these variables are often calculated with the above variables

  # loop through each variable that will need to be defined
  for (i in 1:length(zoning_defs)){
    var_name <- names(zoning_defs)[[i]]

    var_list <- zoning_defs[[i]]

    # loop through each array item under the variable
    notmet <- 0 # start to track number of items that didn't meet the conditions
    for (j in 1:length(var_list)){
      condition_list <- var_list[[j]]$condition
      condition <- paste0("(",condition_list,")", collapse = " and ")
      condition <- gsub("and","&",condition)
      condition <- gsub("or","|",condition)

      evaluated <- tryCatch({
        eval(parse(text = condition))
      }, error = function(e) {
        return("error")
      })

      if (evaluated == "error"){
        notmet <- notmet + 1
        next
      }


      if (evaluated){
        expr <- var_list[[j]]$expression
        evaluated_expr <- tryCatch({
          eval(parse(text = expr))
        }, error = function(e) {
          return("error")
        })

        if (evaluated_expr == "error"){
          stop(paste("unable to evaluate variable:", var_name))
        }
        value <- evaluated_expr
        break
      }

    }

    if (notmet == length(var_list)){
      stop(paste("No conditions met. Unable to find",var_name, "variable"))
    }

    vars_df[[var_name]] <- value

  } # end loop through each variable that will need to be defined.


  return(vars_df)
}
