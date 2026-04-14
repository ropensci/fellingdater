# Compute Student's t-statistics from correlation coefficients

This function computes correlation coefficients and their corresponding
Student's t-statistics between all pairs of series from two tree-ring
datasets.

## Usage

``` r
trs_tSt(x, y = NULL, min_overlap = 30, as_df = FALSE)
```

## Arguments

- x:

  A data frame of test tree-ring series in `rwl` format (years as
  rownames, series as columns). All columns must be numeric.

- y:

  A data frame of reference tree-ring series in `rwl` format.

- min_overlap:

  Integer. Minimum number of overlapping years required between series
  pairs to compute statistics. Must be \>= 3. Default is 30.

- as_df:

  Logical. If `TRUE`, returns results as data frames. If `FALSE`,
  returns as matrices. Default is `TRUE`.

## Value

A list containing:

- r:

  Correlation coefficients between series pairs

- t:

  Student's t-statistics

- overlap:

  Number of overlapping years between series pairs

## Details

The function computes Pearson correlation coefficients between all pairs
of series from the two input datasets, then converts these to Student's
t-statistics using the formula: t = r \* sqrt(n-2) / sqrt(1-^2), where n
is the number of overlapping observations.

## Examples

``` r
# Create sample data,
trs <- trs_pseudo_rwl(n_series = 5, series_length = c(80, 100), end_date = c(2010, 2020))
trs_tSt(trs)$t_St
#>       trs_1 trs_2 trs_3 trs_4 trs_5
#> trs_1   Inf  4.24  8.56  9.29  7.15
#> trs_2  4.24   Inf  5.71  6.69  5.44
#> trs_3  8.56  5.71   Inf 12.70  9.56
#> trs_4  9.29  6.69 12.70   Inf 11.06
#> trs_5  7.15  5.44  9.56 11.06   Inf
```
