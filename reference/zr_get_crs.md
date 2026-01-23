# Find EPSG code for State Plane CRS

`zr_get_crs()` uses state plane geometry to find an appropriate NAD83
crs for the input geometry

## Usage

``` r
zr_get_crs(geom_data, large_area = FALSE)
```

## Arguments

- geom_data:

  Either a simple feature collection or a path to a file containing
  geospatial data

- large_area:

  Set this to `TRUE` if your data may cross multiple state planes. It
  will be a bit longer but find the state plane that covers it the best

## Value

Returns the appropriate epsg code as an integer

## Details

The state plane data was compiled using [ArcGIS
Hub](https://hub.arcgis.com/datasets/esri::usa-state-plane-zones-nad83/explore)
and the [epsg.io](https://epsg.io/) website.

## Examples

``` r
file <- zr_example_files("Paradise.zoning")
zr_get_crs(file)
#> [1] 32138
```
