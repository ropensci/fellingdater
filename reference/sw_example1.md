# Example dataset 1

A dataset in which all series have preserved sapwood rings.

## Usage

``` r
sw_example1
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
sw_example1
#>   series last n_sapwood waneyedge
#> 1 trs_01 1000         5     FALSE
#> 2 trs_02 1009        10     FALSE
#> 3 trs_03 1007        15     FALSE
#> 4 trs_04 1005        16     FALSE
#> 5 trs_05 1010         8     FALSE
```
