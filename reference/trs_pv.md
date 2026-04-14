# Computes statistics to describe parallel variations between two rwl-style data frames

Compares tree-ring series in two data frames using Gleichläufigkeit
(GLK) and Synchronous Growth Changes (SGC), adapted from the `dplR`
package.

## Usage

``` r
trs_pv(x, y = NULL, min_overlap = 50, prob = TRUE, as_df = FALSE)
```

## Arguments

- x:

  A data frame of test tree-ring series (`rwl` format: years in rows,
  series in columns).

- y:

  A data frame of reference series (`rwl` format).

- min_overlap:

  Integer. Minimum number of overlapping years required for computing
  GLK/SGC statistics. Default is 50.

- prob:

  Logical. If TRUE (default), computes two-tailed p-values for GLK and
  SGC.

- as_df:

  Logical. If TRUE, returns a long-format data.frame with all
  combinations and results. Default is FALSE.

## Value

If `as_df = FALSE`, a list with components:

- glk:

  Matrix of Gleichläufigkeit values.

- glk_p:

  Matrix of GLK p-values (if `prob = TRUE`).

- sgc:

  Matrix of synchronous growth change values.

- ssgc:

  Matrix of semi-synchronous growth change values.

- sgc_p:

  Matrix of SGC p-values (if `prob = TRUE`).

- overlap:

  Matrix of the number of overlapping years used in each comparison.

If `as_df = TRUE`, returns a data frame with columns: `series`,
`reference`, `glk`, `glk_p`, `sgc`, `ssgc`, `sgc_p`, and `overlap`.

## Examples

``` r
# Create sample data,
trs <- trs_pseudo_rwl(n_series = 5, series_length = c(80, 100), end_date = c(2010, 2020))
trs_pv(trs)$sgc
#>       trs_1 trs_2 trs_3 trs_4 trs_5
#> trs_1 100.0  59.5  71.0  69.1  69.8
#> trs_2  59.5 100.0  66.7  63.3  60.8
#> trs_3  71.0  66.7 100.0  67.7  73.1
#> trs_4  69.1  63.3  67.7 100.0  68.8
#> trs_5  69.8  60.8  73.1  68.8 100.0
```
