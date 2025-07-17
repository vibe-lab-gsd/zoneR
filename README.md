
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zoneR

<!-- badges: start -->

<!-- badges: end -->

Analyze the housing capacity of cities using Open Zoning Feed
Specification (OZFS) data.

The main function of zoneR is `zr_run_zoning_checks()` which reads in an
OZFS `.bldg`, `.parcel`, and `.zoning` file. It calculates the zoning
requirements for each parcel, checks them against the building
characteristics, and returns a data frame stating if the building is
allowed on the parcel.

Inside the `zr_run_zoning_checks()` function, different variables are
made from the OZFS files and used in other functions to perform the
checks. Some of the main variables are listed below.

- **bldg_data:** The `.bldg` file read in as a list using
  `rjson::fromJSON()`

- **parcel_dims:** Created from the `zr_get_parcel_dims()` function. A
  simple features data frame with all the centroid and dimensional data
  from the `.parcel` file. It contains one row per parcel.

- **parcel_data:** One row of the parcel_dims data frame representing a
  unique parcel

- **parcel_geo:** Created from the `zr_get_parcel_geo()` function. A
  simple features data frame containing the geometry of each parcel side
  without the centroid or dimensional data.

- **zoning_data:** The data in the `.zoning` file read in as a simple
  features data frame using `sf::st_read()`.

- **district_data:** One row of the zoning_data data frame representing
  a unique district.

## Installation

You can install the development version of zoneR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("vibe-lab-gsd/zoneR")
```

## Example

This is a basic example which shows you how to solve a common problem:
