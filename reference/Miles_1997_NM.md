# Miles 1997 sapwood data set.

Sapwood data set for the Northern Midland counties (U.K.) - Cheshire,
Staffordshire, West Midlands, Northamptonshire, Cambridgeshire, and
everything to the north - published by Miles in 1997.

## Usage

``` r
Miles_1997_NM
```

## Format

A tibble of 2 variables:

- n_sapwood:

  number of sapwood rings

- count:

  number of times n_sapwood was observed

## Source

Miles D. 1997. The interpretation, presentation and use of tree-ring
dates. Vernacular architecture 28, 40–56.
<https://doi.org/10.1179/030554797786050563>

## Details

    sample size = 295 observations

## Examples

``` r
sw_data_info("Miles_1997_NM")
#> $data
#> [1] "Miles_1997_NM"
#> 
#> $citation
#> [1] "Miles D. 1997. The interpretation, presentation and use of tree-ring dates. Vernacular architecture 28, 40–56. DOI: https://doi.org/10.1179/030554797786050563."
#> 
#> $area
#> [1] "U.K.: Nothern Midland counties, Cheshire, Staffordshire, West Midlands, Northamptonshire, Cambridgeshire, and everything to the north."
#> 
#> $n_observations
#> [1] 295
#> 
#> $summary_raw_data
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   10.00   18.00   23.00   23.97   28.50   59.00 
#> 

sw_model("Miles_1997_NM", plot = TRUE)

```
