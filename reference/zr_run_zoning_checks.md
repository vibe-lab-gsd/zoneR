# Find where a building is allowed to be built

`zr_run_zoning_checks()` checks the building information against all the
zoning constraints to see which parcels will allow the building.

## Usage

``` r
zr_run_zoning_checks(
  bldg_file,
  parcel_files,
  zoning_files,
  detailed_check = FALSE,
  print_checkpoints = TRUE,
  checks = possible_checks,
  save_to = NULL
)
```

## Arguments

- bldg_file:

  The path to the OZFS \*.bldg

- parcel_files:

  The path to an OZFS `.parcel` file, or the path to a folder containing
  OZFS `.parcel` files

- zoning_files:

  The path to an OZFS `.zoning` file, or the path to a folder containing
  OZFS `.zoning` files

- detailed_check:

  When TRUE, every parcel passes through each check no matter the
  result, and it takes more time. When FALSE, subsequent checks are
  skipped as soon as one check reads FALSE

- print_checkpoints:

  When TRUE, runtimes and other info will be printed at certain points
  throughout the function.

- checks:

  A list of all the checks that should take place. The default is to
  check for every constraint possible in the OZFS. These constraints can
  found in the package data `possible_checks`. Note, if a zoning file
  doesn't have zoning info for one of the constraints listed in the
  checks variable, then it is assumed that building characteristic is
  allowed.

- save_to:

  The path or folder directory where you want to save the results as a
  geojson file.

## Value

A simple features data frame with geometry for the centroid of each
parcel and columns to show info on allowance of the building.

## Zoning Checks

The checks that can take place in `zr_run_zoning_checks()` are included
in the table below. Most checks are not required and only checked if
specified by the user in the `checks` argument. All optional checks
possible are stored in the `possible_checks` data which is the default
value of the `checks` argument. A few checks are built in to the
function and, therefore, not a part of the `checks` argument.

The resulting data frame of `zr_run_zoning_checks()` will have a
"reason" column with the names of any checks that caused a FALSE or a
MAYBE building allowance. For more details on what the reasons column
means, see the package data `reasons`.

## Examples

``` r
zoning_file <- zr_example_files("Paradise.zoning")
parcel_file <- zr_example_files("Paradise.parcel")
bldg_file <- zr_example_files("2_fam.bldg")

# only checking against height constraints for speed
just_check_height <- zr_run_zoning_checks(bldg_file = bldg_file,
                                          parcel_files = parcel_file,
                                          zoning_files = zoning_file,
                                          checks = "height")
#> ___data_prep___(0.2 sec)
#> 
#> ___get_zoning_req___(2.6 sec)
#> 
#> ___initial_checks___(1.4 sec)
#> 97 parcels are TRUE or MAYBE
#> 
#> _____summary_____
#> total runtime: 4.2 sec (0.07 min)
#> 97 / 421 parcels allow the building
```
