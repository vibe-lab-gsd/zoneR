# Table of zoneR result's False and Maybe reasons

This table describes what each name means when it is listed as a reason
for a MAYBE or FALSE value in the building check.

## Usage

``` r
reasons
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 31
rows and 3 columns.

- short_reason:

  The reason given in the results of
  [`zr_run_zoning_checks()`](https://vibe-lab-gsd.github.io/zoneR/reference/zr_run_zoning_checks.md)

- false_explain:

  What the reason means when the value is FALSE (i.e. when the building
  is not allowed)

- maybe_explain:

  What the reason means when the value is MAYBE (i.e. when the building
  might be allowed)

## Source

Generated internally by zoneR
