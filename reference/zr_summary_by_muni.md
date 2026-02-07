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
