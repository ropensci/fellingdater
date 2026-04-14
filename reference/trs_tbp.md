# Compute Baillie & Pilcher-style t-values between all series in two rwl-style data frames

This function computes crossdating t-statistics between all series in
two datasets using the Baillie & Pilcher (1973) method. Series are
standardized using a 5-year centered moving average and log-transformed
before computing correlations and t-values.

## Usage

``` r
trs_tbp(x, y = NULL, min_overlap = 50, as_df = FALSE, transform = TRUE)
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

  Logical. If `TRUE`, applies Baillie & Pilcher transformation via
  [`trs_tbp_transform()`](https://ropensci.github.io/fellingdater/reference/trs_tbp_transform.md).
  If `FALSE`, assumes input data is already transformed. Default is
  `TRUE`.

## Value

Depending on `as_df` parameter:

- If `as_df = FALSE` (default): A list containing:

  - `t_BP`: Matrix of t-values with test series as rows, reference
    series as columns

  - `overlap`: Matrix of overlap counts (number of common years) with
    same dimensions

- If `as_df = TRUE`: A data frame with columns:

  - `series`: Name of test series

  - `reference`: Name of reference series

  - `t_BP`: Baillie & Pilcher t-value

  - `overlap`: Number of overlapping years

## Details

The Baillie & Pilcher method involves:

1.  Alignment of series by common years

2.  Application of 5-year centered moving average to each series

3.  Log transformation: \\\log(100 \times \text{value} / \text{moving
    average})\\

4.  Computation of Pearson correlation between transformed series

5.  Calculation of t-statistic: \\t = r \sqrt{(n-4-2)} / \sqrt{1-r^2}\\

Where \\n\\ is the number of overlapping observations and is adjusted by
subtracting 4 to account for degrees of freedom lost in the moving
average.

Negative correlations are set to 0, and perfect correlations (\|r\| = 1)
result in infinite t-values.

## References

Baillie, M.G.L. & Pilcher, J.R. (1973). A simple crossdating program for
tree-ring research. *Tree-Ring Bulletin*, 33, 7–14.

## See also

[`trs_tbp_transform`](https://ropensci.github.io/fellingdater/reference/trs_tbp_transform.md)
for the transformation function

## Examples

``` r
if (FALSE) { # \dontrun{
# Create sample data
rwl_test <- trs_pseudo_rwl(n_series = 3, series_length = 100, end_date = 2020)
rwl_ref <- trs_pseudo_rwl(n_series = 2, series_length = 80, end_date = 2015)

# Compute t-values (matrix format)
result <- trs_tbp(rwl_test, rwl_ref, min_overlap = 30)
print(result$t_BP)
print(result$overlap)

# Compute t-values (data frame format)
result_df <- trs_tbp(rwl_test, rwl_ref, min_overlap = 30, as_df = TRUE)
print(result_df)

# Self-comparison within single dataset
self_comparison <- trs_tbp(rwl_test, min_overlap = 40)
} # }
```
