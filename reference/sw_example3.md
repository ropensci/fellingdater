# Example dataset 3

A dataset with multiple exact felling dates.

## Usage

``` r
sw_example3
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
sw_example3
#>   series last n_sapwood waneyedge
#> 1 trs_11 1000         5      TRUE
#> 2 trs_12 1005        10      TRUE
#> 3 trs_13 1008        NA      TRUE
#> 4 trs_14 1000         1      TRUE
#> 5 trs_15 1010        NA      TRUE
```
