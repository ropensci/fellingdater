# Miles 1997 sapwood data set.

Sapwood data set for Wales and border counties (U.K.), Shropshire,
Hereford and Worcesterthe, published by Miles in 1997.

## Usage

``` r
Miles_1997_WBC
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

    sample size = 219 observations

## Examples

``` r
sw_data_info("Miles_1997_WBC")
#> $data
#> [1] "Miles_1997_WBC"
#> 
#> $citation
#> [1] "Miles D. 1997. The interpretation, presentation and use of tree-ring dates. Vernacular architecture 28, 40–56. DOI: https://doi.org/10.1179/030554797786050563."
#> 
#> $area
#> [1] "U.K.: Wales and border counties, Shropshire, Hereford and Worcester."
#> 
#> $n_observations
#> [1] 219
#> 
#> $summary_raw_data
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    7.00   16.00   20.00   20.99   25.00   49.00 
#> 

sw_model("Miles_1997_WBC", plot = TRUE)

```
