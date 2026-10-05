# Merge an overlay and a base district's requirements

Takes the overlay district requirements and the base districts
requirements, and creates one data frame of the proper constraint values
to be used in the analysis.

## Usage

``` r
zr_merge_overlay_reqs(base_reqs, overlay_reqs, overlay_type)
```

## Arguments

- base_reqs:

  The data frame representing the zoning requirements for the base
  district that was created with
  [`zr_get_zoning_req()`](https://vibe-lab-gsd.github.io/zoneR/reference/zr_get_zoning_req.md)

- overlay_reqs:

  The data frame representing the zoning requirements for the overlay
  district that was created with
  [`zr_get_zoning_req()`](https://vibe-lab-gsd.github.io/zoneR/reference/zr_get_zoning_req.md)

- overlay_type:

  String stating whether the overlay is of type "restrict", "relax", or
  "replace"

## Value

One data frame that is a merger of the two input data frames according
to respective overly rules

## Examples

``` r
base_reqs <- data.frame(constraint_name = c("lot_area","setback_front"),
                        min_value = I(list(list(0.17), list(5))),
                        max_value = I(list(list(1.5), list(30))))
overlay_reqs <- data.frame(constraint_name = c("lot_area","setback_front"),
                        min_value = I(list(list(0.2), list(10))),
                        max_value = I(list(list(2), list(35))))

zr_merge_overlay_reqs(base_reqs, overlay_reqs, "restrict")
#>   constraint_name min_value max_value min_ovly max_ovly
#> 1        lot_area       0.2       1.5 restrict     base
#> 2   setback_front        10        30 restrict     base

zr_merge_overlay_reqs(base_reqs, overlay_reqs, "relax")
#>   constraint_name min_value max_value min_ovly max_ovly
#> 1        lot_area      0.17         2     base    relax
#> 2   setback_front         5        35     base    relax

zr_merge_overlay_reqs(base_reqs, overlay_reqs, "replace")
#>   constraint_name min_value max_value min_ovly max_ovly
#> 1        lot_area       0.2         2  replace  replace
#> 2   setback_front        10        35  replace  replace
```
