# Example dataset 7

A test dataset for sw_sum().

## Usage

``` r
sw_example7
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
sw_example7
#>   series last n_sapwood waneyedge
#> 1 trs_34 1000         5     FALSE
#> 2 trs_35 1009        10     FALSE
#> 3 trs_36 1007        15      TRUE
#> 4 trs_37 1007        16      TRUE
#> 5 trs_38 1010         8     FALSE
#> 6 trs_39 1020         0     FALSE
#> 7 trs_40 1025        10     FALSE
#> 8 trs_41 1050        NA      TRUE
#> 9 trs_42 1035         1     FALSE
```
