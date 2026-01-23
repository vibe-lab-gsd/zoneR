# Get all OZFS variables in one table

Get all OZFS variables in one table

## Usage

``` r
zr_get_variables(bldg_data, parcel_data, district_data, zoning_data)
```

## Arguments

- bldg_data:

  either the file path to an OZFS \*.bldg file or a list created from
  the the \*.bldg file using
  [`rjson::fromJSON`](https://rdrr.io/pkg/rjson/man/fromJSON.html)

- parcel_data:

  one row of a parcel data frame created from the OZFS \*.parcel file

- district_data:

  one row (representing one district) of a zoning data frame created
  from the OZFS \*.zoning file

- zoning_data:

  either the path to a \*.zoning file or a list created from the the
  \*.zoning file using
  [`rjson::fromJSON`](https://rdrr.io/pkg/rjson/man/fromJSON.html)

## Value

a one-row data frame with a column for each variable

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

# get parcel_geo and parcel_dims
parcel_geo <- zr_get_parcel_geo(parcels_sf)
parcel_dims <- zr_get_parcel_dims(parcels_sf)

# use parcel_dims to create parcel_df with a zoning_id column
parcel_df <- zr_find_district_idx(parcel_dims, zoning_sf)

# choose just one parcel for the example
parcel_data <- parcel_df[parcel_df$parcel_id == "Wise_County_combined_parcel_10300",]
# get the row of the district that parcel is apart of
district_data <- zoning_sf[parcel_data$zoning_id,]

# get variables
vars <- zr_get_variables(bldg_data, parcel_data, district_data, zoning_data)
```
