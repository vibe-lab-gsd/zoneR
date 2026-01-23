# Is the building allowed base on use_type?

Campares the building use type with the permitted use types in the
zoning code and returns TRUE or FALSE

## Usage

``` r
zr_check_res_type(vars, district_data)
```

## Arguments

- vars:

  the result from the
  [`zr_get_variables()`](https://vibe-lab-gsd.github.io/zoneR/reference/zr_get_variables.md)
  function. If this data frame is supplied, bldg_data, parcel_data, and
  zoning_data are not needed.

- district_data:

  one row (representing one district) of a zoning data frame created
  from the OZFS \*.zoning file

## Value

TRUE or FALSE

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

# we see that a 2-family building is not allowed in this parcel
res_type_check <- zr_check_res_type(vars, district_data)
```
