# Is it formatted to OZFS standards?

`zr_ozfs_is_valid()` TRUE or FALSE stating whether or not the files are
properly formatted according to OZFS standards. Use
[zr_ozfs_validate](https://vibe-lab-gsd.github.io/zoneR/reference/zr_ozfs_validate.md)
for a detailed list of why files don't meet the standard.

## Usage

``` r
zr_ozfs_is_valid(list_of_files)
```

## Arguments

- list_of_files:

  A list of file paths to OZFS \*.zoning files

## Value

TRUE or FALSE stating whether data is in valid OZFS format or not

## Examples

``` r
correct_file <- zr_example_files("Paradise.zoning")
incorrect_file <- zr_example_files("Paradise.parcel")

# correct OZFS
zr_ozfs_validate(correct_file)
#> There was 1 file with errors or warnings 
#> 
#> $Paradise.zoning
#> $Paradise.zoning$warnings
#>  [1] "A : no planned_dev field"   "A : no overlay field"      
#>  [3] "R-1 : no planned_dev field" "R-1 : no overlay field"    
#>  [5] "R-2 : no planned_dev field" "R-2 : no overlay field"    
#>  [7] "B-1 : no planned_dev field" "B-1 : no overlay field"    
#>  [9] "I-1 : no planned_dev field" "I-1 : no overlay field"    
#> [11] "I-2 : no planned_dev field" "I-2 : no overlay field"    
#> [13] "MU : no planned_dev field"  "MU : no overlay field"     
#> 
#> 

zr_ozfs_is_valid(correct_file)
#> There was 1 file with errors or warnings 
#> 
#> [1] FALSE

# incorrect OZFS
zr_ozfs_is_valid(incorrect_file)
#> There was 1 file with errors or warnings 
#> 
#> [1] FALSE
```
