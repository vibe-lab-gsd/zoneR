# Create data frame with unit info

`zr_get_unit_info()` takes a json representing a building and returns a
data frame with data for each unit in the building.

## Usage

``` r
zr_get_unit_info(bldg_data)
```

## Arguments

- bldg_data:

  either the file path to an OZFS \*.bldg file or a list created from
  the the \*.bldg file using
  [`rjson::fromJSON`](https://rdrr.io/pkg/rjson/man/fromJSON.html)

## Value

a data frame with information for each unit in the building

## Examples

``` r
bldg_file <- zr_example_files("2_fam.bldg")

zr_get_unit_info(bldg_file)
#>   fl_area bedrooms qty
#> 1    1563        3   2
```
