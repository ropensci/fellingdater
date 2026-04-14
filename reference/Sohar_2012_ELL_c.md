# Sohar et al. 2012 sapwood data set.

Sapwood data set for Eastern Estonia, Latvia, Lithuania, published by
Sohar et al. in 2012.

## Usage

``` r
Sohar_2012_ELL_c
```

## Format

A tibble of 2 variables:

- n_sapwood:

  number of sapwood rings

- count:

  number of times n_sapwood was observed

## Source

Sohar K., Vitas A. & Läänelaid A. 2012. Sapwood estimates of pedunculate
oak (*Quercus robur* L.) in eastern Baltic, Dendrochronologia 30.1,
49–56. <https://doi.org/10.1016/j.dendro.2011.08.001>

## Details

    sample size = 562 observations
    !!! sapwood determined by _color_

## Examples

``` r
sw_data_info("Sohar_2012_ELL_c")
#> $data
#> [1] "Sohar_2012_ELL_c"
#> 
#> $citation
#> [1] "Sohar K., Vitas A. & Läänelaid A. 2012. Sapwood estimates of pedunculate oak (Quercus robur L.) in eastern Baltic, Dendrochronologia 30.1, 49–56. DOI: https://doi.org/10.1016/j.dendro.2011.08.001"
#> 
#> $area
#> [1] "Eastern Estonia, Latvia, Lithuania (sapwood determined by color)."
#> 
#> $n_observations
#> [1] 562
#> 
#> $summary_raw_data
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    2.00   10.00   12.00   11.69   13.00   27.00 
#> 

sw_model("Sohar_2012_ELL_c", plot = TRUE)

```
