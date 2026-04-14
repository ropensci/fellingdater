# Example dataset 4

A combination of series with and without sapwood rings.

## Usage

``` r
sw_example4
```

## Format

A tibble of 4 variables:

- series:

  unique ID of the tree-ring series

- last:

  calendar year assigned to the last measured ring

- n_sapwood:

  number of observed sapwood rings

- waneyedge:

  waney edge present TRUE/FALSE

## Examples

``` r
sw_example4
#>   series last n_sapwood waneyedge
#> 1 trs_16 1000         5     FALSE
#> 2 trs_17 1005        10     FALSE
#> 3 trs_18 1005        NA     FALSE
#> 4 trs_19 1020         1     FALSE
#> 5 trs_20 1040         0     FALSE
```
