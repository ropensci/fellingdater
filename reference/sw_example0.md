# Example dataset 0

A dataset in which all series have preserved sapwood. Unconventional
variable names.

## Usage

``` r
sw_example0
```

## Format

A tibble of 4 variables:

- trs:

  unique ID of the tree-ring series

- end:

  calendar year assigned to the last measured ring

- swr:

  number of observed sapwood rings

- bark:

  waney edge present TRUE/FALSE

## Examples

``` r
sw_example0
#>      trs  end swr  bark
#> 1 trs_01 1000   5 FALSE
#> 2 trs_02 1009  10 FALSE
#> 3 trs_03 1007  15 FALSE
#> 4 trs_04 1005  16 FALSE
#> 5 trs_05 1010   8 FALSE
```
