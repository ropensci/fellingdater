# Pilcher 1987 sapwood data set.

Sapwood data set for northern France , published by Pilcher in 1987.

## Usage

``` r
Pilcher_1987
```

## Format

A tibble of 2 variables:

- n_sapwood:

  number of sapwood rings

- count:

  number of times n_sapwood was observed

## Source

Pilcher J.R. 1987. A 700 year dating chronology for northern France.
Applications of tree-ring studies. Current research in dendrochronology
and related subjects. BAR International Series 333, 127–139.

## Details

    sample size = 219 observations

## Examples

``` r
sw_data_info("Pilcher_1987")
#> $data
#> [1] "Pilcher_1987"
#> 
#> $citation
#> [1] "Pilcher J.R. 1987. A 700 year dating chronology for northern France. Applications of tree-ring studies. Current research in dendrochronology and related subjects. BAR International Series 333, 127–139."
#> 
#> $area
#> [1] "Northern France"
#> 
#> $n_observations
#> [1] 116
#> 
#> $summary_raw_data
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   12.00   22.00   26.00   26.72   31.00   49.00 
#> 

sw_model("Pilcher_1987", plot = TRUE)

```
