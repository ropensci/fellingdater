# Weitz 2025 sapwood data set.

Sapwood data set for the Brussels region in Belgium, published by Weitz
et al. in 2025.

## Usage

``` r
Weitz_2025
```

## Format

A tibble of 2 variables:

- n_sapwood:

  number of sapwood rings

- count:

  number of times n_sapwood was observed

## Source

Weitz A., Haneca K. & Hoffsummer P. (in press): Estimating sapwood
counts for historical oak timbers from Brussels, Belgium (13th-18th C),
International Journal of Wood Culture.

## Details

    sample size = 443 observations

## Examples

``` r
sw_data_info("Weitz_2025")
#> $data
#> [1] "Weitz_2025"
#> 
#> $citation
#> [1] "Weitz A., Haneca K. & Hoffsummer P. (in press): Estimating sapwood counts for historical oak timbers from Brussels, Belgium (13th-18th C), International Journal of Wood Culture."
#> 
#> $area
#> [1] "Wider Brussels region (Belgium)."
#> 
#> $n_observations
#> [1] 443
#> 
#> $summary_raw_data
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    4.00   12.00   15.00   15.47   18.00   36.00 
#> 

sw_model("Weitz_2025", plot = TRUE)

```
