# Compute Hollstein-style t-values between all series in two rwl-style data frames

This function calculates Hollstein (1980) t-values for all pairwise
comparisons between tree-ring series in two data frames, using
log-transformed growth ratios. The method computes year-to-year growth
ratios and applies correlation-based t-statistics for crossdating
analysis.

## Usage

``` r
trs_tho(x, y = NULL, min_overlap = 50, as_df = FALSE, transform = TRUE)
```

## Arguments

- x:

  A data frame of test tree-ring series in `rwl` format (years as
  rownames, series as columns). All columns must be numeric.

- y:

  A data frame of reference tree-ring series in `rwl` format. If `NULL`,
  uses `x` for self-comparison. Default is `NULL`.

- min_overlap:

  Integer. Minimum number of overlapping years required between series
  pairs to compute t-values. Must be \>= 3. Default is 50.

- as_df:

  Logical. If `TRUE`, returns results in long-format data frame. If
  `FALSE`, returns list of matrices. Default is `FALSE`.

- transform:

  Logical. If `TRUE`, applies Hollstein transformation (log growth
  ratios). If `FALSE`, assumes input data is already transformed.
  Default is `TRUE`.

## Value

Depending on `as_df` parameter:

- If `as_df = FALSE` (default): A list containing:

  - `t_Ho`: Matrix of Hollstein t-values with test series as rows,
    reference series as columns

  - `overlap`: Matrix of overlap counts (number of common years) with
    same dimensions

- If `as_df = TRUE`: A data frame with columns:

  - `series`: Name of test series

  - `reference`: Name of reference series

  - `t_Ho`: Hollstein t-value

  - `overlap`: Number of overlapping years

## Details

The Hollstein method involves:

1.  Alignment of series by common years

2.  Computation of log-transformed growth ratios: \\100 \times
    \log\_{10}(x_t / x\_{t-1})\\

3.  Calculation of Pearson correlation between transformed series

4.  Computation of t-statistic: \\t = r \sqrt{n-3} / \sqrt{1-r^2}\\

Where \\n\\ is the number of overlapping observations (original years,
not growth ratios). The degrees of freedom are adjusted by subtracting 3
to account for the growth ratio transformation.

Negative correlations are set to 0, and perfect correlations (\|r\| ≥ 1)
result in infinite t-values to handle floating-point precision issues.

## References

Hollstein, E. (1980). *Mitteleuropäische Eichenchronologie*. Verlag
Philipp von Zabern, Mainz.

## See also

[`trs_tbp`](https://ropensci.github.io/fellingdater/reference/trs_tbp.md)
for Baillie & Pilcher t-values

## Examples

``` r
if (FALSE) { # \dontrun{
# Create sample data
rwl_test <- trs_pseudo_rwl(n_series = 3, series_length = 100, end_date = 2020)
rwl_ref <- trs_pseudo_rwl(n_series = 2, series_length = 80, end_date = 2015)

# Compute Hollstein t-values (matrix format)
result <- trs_tho(rwl_test, rwl_ref, min_overlap = 30)
print(result$t_Ho)
print(result$overlap)

# Compute t-values (data frame format)
result_df <- trs_tho(rwl_test, rwl_ref, min_overlap = 30, as_df = TRUE)
print(result_df)

# Self-comparison within single dataset
self_comparison <- trs_tho(rwl_test, min_overlap = 40)
} # }
```
