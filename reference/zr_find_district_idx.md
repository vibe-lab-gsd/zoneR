# Find corresponding zoning district

`zr_find_district_idx()` uses
[`sf::st_join()`](https://r-spatial.github.io/sf/reference/st_join.html)
to adds a new column to the parcels data that contains the zoning_sf row
index of the district the parcel is in

## Usage

``` r
zr_find_district_idx(
  parcels_centroids_sf,
  zoning_sf,
  idx_col_name = "zoning_id"
)
```

## Arguments

- parcels_centroids_sf:

  An sf object with the parcel centroids

- zoning_sf:

  A simple features object with a row for each zoning district and
  columns holding the geojson formatted zoning requirements of each
  district.

- idx_col_name:

  The name of the new column that will be added with the district index

## Value

The same parcels centroids object that was input but with an additional
column stating the row number of the corresponding zoning_sf district

## Examples

``` r
# preparing  all the OZFS files to run functions
zoning_file <- zr_example_files("Paradise.zoning")
parcel_file <- zr_example_files("Paradise.parcel")

# getting zoning file as simple features object
zoning_sf <- sf::st_read(zoning_file, quiet = TRUE)

# getting parcel file as simple features object
parcels_sf <- sf::st_read(parcel_file, quiet = TRUE)

# get parcel_dims
parcel_dims <- zr_get_parcel_dims(parcels_sf)

# use parcel_dims to create parcel_df with a zoning_id column
parcel_df <- zr_find_district_idx(parcel_dims, zoning_sf)

# notice the zoning_id column
head(parcel_df)
#> Simple feature collection with 6 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -97.69917 ymin: 33.1448 xmax: -97.69153 ymax: 33.15607
#> Geodetic CRS:  WGS 84
#>                           parcel_id lot_width lot_depth   lot_area lot_type
#> 1     Wise_County_combined_parcel_1    1.0000    1.0000 66.1724481  regular
#> 2 Wise_County_combined_parcel_10300  225.5188  603.3620  1.9954889   corner
#> 3 Wise_County_combined_parcel_10450  284.8850  655.8875  4.2351432   corner
#> 4 Wise_County_combined_parcel_10451  105.1301  109.9494  0.2620239  regular
#> 5 Wise_County_combined_parcel_10452   99.8650  109.8487  0.2517562   corner
#> 6 Wise_County_combined_parcel_10464    1.0000    1.0000 97.5398037  regular
#>   zoning_id                   geometry
#> 1         2 POINT (-97.69524 33.14755)
#> 2         2 POINT (-97.69382 33.15607)
#> 3         2  POINT (-97.69415 33.1448)
#> 4         2 POINT (-97.69156 33.14562)
#> 5         2 POINT (-97.69153 33.14699)
#> 6         2 POINT (-97.69917 33.15142)
```
