# Summarize the reasons for maybe results

[`zr_summary_by_muni()`](https://vibe-lab-gsd.github.io/zoneR/reference/zr_summary_by_muni.md)
summarizes results reasons given for maybe results from the output of
zr_run_zoning_checks()

## Usage

``` r
zr_summary_false(detailed_result, muni = "All")
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
#> ___get_zoning_req___(2.3 sec)
#> 
#> ___initial_checks___(1.2 sec)
#> 97 parcels are TRUE or MAYBE
#> 
#> Error in dplyr::mutate(dplyr::summarise(dplyr::group_by(dplyr::filter(sf::st_drop_geometry(overlay_df),     has_overlay == "TRUE"), parcel_id), comb_check = paste(allowed_now,     collapse = " - ")), overlay_check = case_when(grepl("FALSE",     comb_check) ~ "FALSE", grepl("MAYBE", comb_check) ~ "MAYBE",     TRUE ~ "TRUE")): ℹ In argument: `overlay_check = case_when(...)`.
#> Caused by error in `case_when()`:
#> ! could not find function "case_when"

zr_summary_by_muni(just_check_height)
#> Error: object 'just_check_height' not found

zr_summary_false(just_check_height)
#> Error: object 'just_check_height' not found

zr_summary_maybe(just_check_height)
#> Error: object 'just_check_height' not found

```
