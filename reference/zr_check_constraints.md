# Is the building allowed base on zoning constraints?

`zr_check_constraints()` compares the building characteristics against
many of the main constraints in the `.zoning` file. It returns a one-row
data frame with a column for each constraint it checked against.

## Usage

``` r
zr_check_constraints(
  vars,
  zoning_req,
  checks = possible_checks[!possible_checks %in% c("res_type", "unit_size", "bldg_fit",
    "overlay")]
)
```

## Arguments

- vars:

  the result from the
  [`zr_get_variables()`](https://vibe-lab-gsd.github.io/zoneR/reference/zr_get_variables.md)
  function. If this data frame is supplied, bldg_data, parcel_data, and
  zoning_data are not needed.

- zoning_req:

  The output of
  [`zr_get_zoning_req()`](https://vibe-lab-gsd.github.io/zoneR/reference/zr_get_zoning_req.md).
  A data frame with constraint values

- checks:

  A list of all the checks that should take place. The default is every
  check possible. Note, if a zoning file doesn't have zoning info for
  one of the constraints listed in the checks variable, then it is
  assumed that building characteristic is allowed.

## Examples

``` r
# preparing  all the OZFS files to run functions
zoning_file <- zr_example_files("Paradise.zoning")
parcel_file <- zr_example_files("Paradise.parcel")
bldg_file <- zr_example_files("2_fam.bldg")

# getting zoning file as simple features object and as a list
zoning_sf <- sf::st_read(zoning_file, quiet = TRUE)
zoning_data <- rjson::fromJSON(file = zoning_file)
# getting parcel file as simple features object
parcels_sf <- sf::st_read(parcel_file, quiet = TRUE)
# getting bldg file as a list
bldg_data <- rjson::fromJSON(file = bldg_file)

# get parcel_dims
parcel_dims <- zr_get_parcel_dims(parcels_sf)

# use parcel_dims to create parcel_df with a zoning_id column
parcel_df <- zr_find_district_idx(parcel_dims, zoning_sf)

# choose just one parcel for the example
parcel_data <- parcel_df[parcel_df$parcel_id == "Wise_County_combined_parcel_10300",]
# get the row of the district that parcel is apart of
district_data <- zoning_sf[parcel_data$zoning_id,]

# get variables
vars <- zr_get_variables(bldg_data, parcel_data, district_data, zoning_data)

# get zoning requirements
zoning_req <- zr_get_zoning_req(district_data = district_data, vars = vars)

constraints_check <- zr_check_constraints(vars, zoning_req)
```
