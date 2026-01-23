# Isolate side geometry from OZFS parcel data frame

`zr_get_parcel_geo()` takes the data in the `.parcel` file and creates a
simple features data frame with only the geometry representing the
parcel sides.

## Usage

``` r
zr_get_parcel_geo(parcels_file)
```

## Arguments

- parcels_file:

  Either a file path to an OZFS `.parcel` file or a simple features
  object with the same data.

## Value

Returns a simple features object with only the side geometries of the
parcels.

## Parcel File

According to OZFS, the `.parcel` file follows geojson format with
geometry for each feature. The features with parcel side geometry store
information on the parcel id and the side label. The features with
parcel centroid geometry store parcel id information along with the
parcel's width, depth, and area. `zr_get_parcel_geo()` filters this data
into a data frame that just has side geometries and labels.
[`zr_get_parcel_dims()`](https://vibe-lab-gsd.github.io/zoneR/reference/zr_get_parcel_dims.md)
filters this data into a data frame that only has centroid geometries
and the dimensional information.

## Examples

``` r
file <- zr_example_files("Paradise.parcel")

parcel_geo <- zr_get_parcel_geo(file)

# parcels with unknown side lables
head(parcel_geo)
#> Simple feature collection with 6 features and 2 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -97.69668 ymin: 33.14243 xmax: -97.69362 ymax: 33.15241
#> Geodetic CRS:  WGS 84
#>                       parcel_id    side                       geometry
#> 1 Wise_County_combined_parcel_1 unknown LINESTRING (-97.69605 33.15...
#> 2 Wise_County_combined_parcel_1 unknown LINESTRING (-97.69362 33.14...
#> 3 Wise_County_combined_parcel_1 unknown LINESTRING (-97.69619 33.14...
#> 4 Wise_County_combined_parcel_1 unknown LINESTRING (-97.69636 33.14...
#> 5 Wise_County_combined_parcel_1 unknown LINESTRING (-97.69668 33.14...
#> 6 Wise_County_combined_parcel_1 unknown LINESTRING (-97.69663 33.15...

# parcels with labeled sides
tail(parcel_geo)
#> Simple feature collection with 6 features and 2 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -97.69436 ymin: 33.15707 xmax: -97.67817 ymax: 33.15892
#> Geodetic CRS:  WGS 84
#>                             parcel_id          side
#> 1956 Wise_County_combined_parcel_9463 interior side
#> 1957 Wise_County_combined_parcel_9463          rear
#> 1958 Wise_County_combined_parcel_9584 exterior side
#> 1959 Wise_County_combined_parcel_9584 interior side
#> 1960 Wise_County_combined_parcel_9584          rear
#> 1961 Wise_County_combined_parcel_9584         front
#>                            geometry
#> 1956 LINESTRING (-97.69364 33.15...
#> 1957 LINESTRING (-97.69412 33.15...
#> 1958 LINESTRING (-97.67892 33.15...
#> 1959 LINESTRING (-97.67818 33.15...
#> 1960 LINESTRING (-97.67893 33.15...
#> 1961 LINESTRING (-97.67817 33.15...
```
