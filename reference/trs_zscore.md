# Standardize tree-ring series to z-scores

Converts each series (column) in an rwl-style `data.frame` to z-scores
by subtracting the mean and dividing by the standard deviation.

## Usage

``` r
trs_zscore(x)
```

## Arguments

- x:

  A `data.frame` of class `"rwl"`, where each column is a tree-ring
  series.

## Value

A `data.frame` of the same dimensions as `x`, with each column
transformed to z-scores.

## Examples

``` r
rwl <- trs_pseudo_rwl(n_series = 3, series_length = 50, end_date = 1990)
z_rwl <- trs_zscore(rwl)
apply(z_rwl, 2, mean, na.rm = TRUE) # should be ~0
#>        trs_1        trs_2        trs_3 
#> 2.896988e-16 5.145624e-17 4.342663e-16 
apply(z_rwl, 2, sd, na.rm = TRUE) # should be ~1
#> trs_1 trs_2 trs_3 
#>     1     1     1 
```
