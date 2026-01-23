# Get path to zoneR's example files

zoneR has a few example OZFS files stored internally. `zr_example_files`
returns either a list of names of the example files, or the file path of
a specified file.

## Usage

``` r
zr_example_files(file = NULL)
```

## Arguments

- file:

  The name of the example file. If NULL, then a list of names of example
  files will be returned

## Value

Either a list of names of the example files, or a path to the specified
file

## Examples

``` r
zr_example_files()
#> [1] "12_fam.bldg"     "2_fam.bldg"      "4_fam_tall.bldg" "4_fam_wide.bldg"
#> [5] "Paradise.parcel" "Paradise.zoning"
zr_example_files("Paradise.parcel")
#> [1] "/home/runner/work/_temp/Library/zoneR/extdata/Paradise.parcel"
```
