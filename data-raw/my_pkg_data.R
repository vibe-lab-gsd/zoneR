## code to prepare `my_pkg_data` dataset goes here




state_planes_crs <- sf::st_read("inst/extdata/sp_crs.geojson")

usethis::use_data(state_planes_crs, overwrite = TRUE, internal = TRUE)
usethis::use_data(state_planes_crs, overwrite = TRUE)

