# Check if footprint fits in buildable area

`zr_check_fit()` takes a building footprint from bldg_data object and
checks to see if it will fit in the provided buildable area. The
buildable area can be any shape.

## Usage

``` r
zr_check_fit(bldg_data, buildable_area, crs = 3081)
```

## Arguments

- bldg_data:

  A list of data frames with attributes representing a building.

- buildable_area:

  A geometry. Usually of the parcels buildable area calculated from the
  setback requirements.

- crs:

  The projected Coordinate Reference System for the study area. Must be
  in meters.

## Value

Returns TRUE of FALSE stating whether or not the building footprint
would fit in the buildable area.

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
# filter for just that one parcel in parcel_geo
parcel_geo_one <- parcel_geo[parcel_geo$parcel_id == "Wise_County_combined_parcel_10300",]
# get the row of the district that parcel is apart of
district_data <- zoning_sf[parcel_data$zoning_id,]

# get variables
vars <- zr_get_variables(bldg_data, parcel_data, district_data, zoning_data)

# get zoning requirements
zoning_req <- zr_get_zoning_req(district_data = district_data, vars = vars)

parcel_with_setbacks <- zr_add_setbacks(parcel_geo_one, district_data, zoning_req)

buildable_area <- zr_get_buildable_area(parcel_with_setbacks)

fit_check <- zr_check_fit(bldg_data, buildable_area[[2]])

```
