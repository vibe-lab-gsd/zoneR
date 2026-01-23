# State Planes with Projected CRS Data

This data set contains the geometry for each US State Plane Zone along
with the EPSG code for its projected coordinate system. The geometry was
gathered from an arcgis hub data set, and the epsg codes were gathered
from epsg.io

## Usage

``` r
state_planes_crs
```

## Format

Simple feature collection with 121 features and 4 fields

- OBJECTID:

  Row Index

- ZONE:

  Zone Abbreviation

- ZONENAME:

  Zone Name

- EPSG_NAD83:

  Projected CRS EPSG Code

## Source

<https://hub.arcgis.com/datasets/esri::usa-state-plane-zones-nad83/explore>

<https://epsg.io/>
