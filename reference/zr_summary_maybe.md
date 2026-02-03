# Summarize the reasons for maybe results

[`zr_summary_by_muni()`](https://vibe-lab-gsd.github.io/zoneR/reference/zr_summary_by_muni.md)
summarizes results reasons given for maybe results from the output of
zr_run_zoning_checks()

## Usage

``` r
zr_summary_maybe(detailed_result, muni = "All")
```

## Arguments

- detailed_result:

  The output simple features data frame from zr_run_zoning_checks()

- muni:

  The city to summarize data for. Defaults to "All" to aggregate across
  all cities include in the results file.

## Value

A tibble summarizing the reasons for maybe values.

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
#> ___get_zoning_req___(2.6 sec)
#> 
#> ___initial_checks___(1.4 sec)
#> 97 parcels are TRUE or MAYBE
#> 
#> _____summary_____
#> total runtime: 4.3 sec (0.07 min)
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
