# Example dataset 2

A dataset in which one series has an exact felling date (= waney edge
present).

## Usage

``` r
sw_example2
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
sw_example2
#>   series last n_sapwood waneyedge
#> 1 trs_06 1000         5     FALSE
#> 2 trs_07 1005        10     FALSE
#> 3 trs_08 1008        NA     FALSE
#> 4 trs_09 1000         1     FALSE
#> 5 trs_10 1010        15      TRUE
```
