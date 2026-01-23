# Isolate dimensional rows from OZFS parcel data

`zr_get_parcel_dims()` takes the data in the `.parcel` file and creates
a simple features data frame with only the geometry representing the
parcel centroids. The data frame contains the dimensional information
related to each parcel.

## Usage

``` r
zr_get_parcel_dims(parcels_file)
```

## Arguments

- parcels_file:

  Either a file path to an OZFS `.parcel` file or a simple features
  object with the same data.

## Value

Returns a simple features object with only dimensional and centroid data
for each parcel. It also adds a lot_type column specifying whether or
not it is a corner parcel.

## Parcel File

According to OZFS, the `.parcel` file follows geojson format with
geometry for each feature. The features with parcel side geometry store
information on the parcel id and the side label. The features with
parcel centroid geometry store parcel id information along with the
parcel's width, depth, and area.
[`zr_get_parcel_geo()`](https://vibe-lab-gsd.github.io/zoneR/reference/zr_get_parcel_geo.md)
filters this data into a data frame that just has side geometries and
labels. `zr_get_parcel_dims()` filters this data into a data frame that
only has centroid geometries and the dimensional information.

## Examples

``` r
file <- zr_example_files("Paradise.parcel")

parcel_dims <- zr_get_parcel_dims(file)
head(parcel_dims)
#> Simple feature collection with 6 features and 5 fields
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
#>                     geometry
#> 1 POINT (-97.69524 33.14755)
#> 2 POINT (-97.69382 33.15607)
#> 3  POINT (-97.69415 33.1448)
#> 4 POINT (-97.69156 33.14562)
#> 5 POINT (-97.69153 33.14699)
#> 6 POINT (-97.69917 33.15142)
```
