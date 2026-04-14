# Miles 1997 sapwood data set.

Sapwood data set for the Southern counties (U.K.), up to and including
Gloucestershire, Warwickshire, Bedfordshire, Suffolk and Norfolk,
published by Miles in 1997.

## Usage

``` r
Miles_1997_SC
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

    sample size = 406 observations

## Examples

``` r
sw_data_info("Miles_1997_SC")
#> $data
#> [1] "Miles_1997_SC"
#> 
#> $citation
#> [1] "Miles D. 1997. The interpretation, presentation and use of tree-ring dates. Vernacular architecture 28, 40–56. DOI: https://doi.org/10.1179/030554797786050563."
#> 
#> $area
#> [1] "U.K.: Southern counties, up to and including Gloucestershire, Warwickshire,\nBedfordshire, Suffolk and Norfolk."
#> 
#> $n_observations
#> [1] 406
#> 
#> $summary_raw_data
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    3.00   16.00   19.00   20.34   24.75   56.00 
#> 

sw_model("Miles_1997_SC", plot = TRUE)

```
