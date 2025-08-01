## code to prepare `my_pkg_data` dataset goes here

# State Planes Data
state_planes_crs <- sf::st_read("inst/extdata/sp_crs.geojson")

usethis::use_data(state_planes_crs, overwrite = TRUE)


# Possible Checks Variable
possible_checks <- c("res_type",
                     "far",
                     "fl_area",
                     "fl_area_first",
                     "fl_area_top",
                     "footprint",
                     "height",
                     "height_eave",
                     "lot_cov_bldg",
                     "lot_area",
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
                     "unit_size_avg",
                     "unit_size",
                     "bldg_fit",
                     "overlay")

usethis::use_data(possible_checks, overwrite = TRUE)
