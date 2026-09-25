# Summarize the results of zr_run_zoning_checks() by municipality

`zr_summary_by_muni()` summarizes results of zr_run_zoning_checks() by
municipality

## Usage

``` r
zr_summary_by_muni(detailed_result)
```

## Arguments

- detailed_result:

  The output simple features data frame from zr_run_zoning_checks()

## Value

A tibble summarizing the number of parcels with TRUE, MAYBE, and FALSE
results by municipality

## Examples

``` r
zoning_file <- zr_example_files("Paradise.zoning")
parcel_file <- zr_example_files("Paradise.parcel")
bldg_file <- zr_example_files("2_fam.bldg")

# only checking against height constraints
just_check_height <- zr_run_zoning_checks(bldg_file = bldg_file,
                                          parcel_files = parcel_file,
                                          zoning_files = zoning_file,
                                          checks = "height")
#> ___data_prep___(0.2 sec)
#> 
#> ___get_zoning_req___(2.5 sec)
#> 
#> ___initial_checks___(1.3 sec)
#> 97 parcels are TRUE or MAYBE
#> 
#> _____summary_____
#> total runtime: 4 sec (0.07 min)
#> 97 / 421 parcels allow the building

zr_summary_by_muni(just_check_height)
#> # A tibble: 2 × 4
#>   muni_name num_allowed num_maybe num_prohib
#>   <chr>           <int>     <int>      <int>
#> 1 Paradise           97         0        324
#> 2 Total              97         0        324

zr_summary_false(just_check_height)
#>          Reason for prohibition Number of parcels
#> 1 Non-compliant building height               324

zr_summary_maybe(just_check_height)
#> No MAYBE values found in results

```
