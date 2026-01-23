# Rotate polygon by specified angle

The `zr_rotate_shape()` function takes a one-row special feature object
and rotates the geometry about the centroid or a specified point.

## Usage

``` r
zr_rotate_shape(shape, angle_degrees, center = NULL)
```

## Arguments

- shape:

  a polygon you want to rotate. Must be a special feature object with
  only one row.

- angle_degrees:

  The angle (in degrees) that you want to rotate the shape

- center:

  The center about which to rotate in the form of XY coordinates.
  Default is NULL, When NULL, rotation is about the centroid.

## Value

Returns the simple feature shape object with rotated geometry

## Examples

``` r
zoning_sf <- sf::st_read(zr_example_files("Paradise.zoning"), quiet = TRUE)
first_district <- zoning_sf[5,]

rotated_district <- zr_rotate_shape(first_district, 45)
#> Warning: st_centroid assumes attributes are constant over geometries
```
