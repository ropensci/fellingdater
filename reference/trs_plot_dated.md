# Plot a dated tree-ring series with a reference series or chronology

Creates a line plot of a dated tree-ring series and a reference
chronology, with optional z-score standardization, statistical
annotations, and visual highlighting of synchronous growth changes
(SCG).

## Usage

``` r
trs_plot_dated(
  x,
  y,
  end_year = NULL,
  zscore = TRUE,
  pv_highlight = TRUE,
  pv_alpha = 0.2,
  show_stats = TRUE,
  labels = TRUE,
  label_size = 3,
  x_breaks = NULL
)
```

## Arguments

- x:

  A `data.frame` (typically of class `'rwl'`) with one column of
  ring-width values. Row names must be numeric and represent calendar
  years.

- y:

  A `data.frame` (typically of class `'rwl'`) with one column of
  ring-width values. Row names must be numeric and represent calendar
  years.

- end_year:

  Optional numeric. If provided, the rownames of `x` will be set to
  match this year for the last ring (using
  [`trs_end_date()`](https://ropensci.github.io/fellingdater/reference/trs_end_date.md)).

- zscore:

  Logical. If `TRUE`, the ring-width series are standardized to z-scores
  before plotting. Defaults to `TRUE`.

- pv_highlight:

  Logical. If `TRUE`, highlights regions of parallel variation
  (synchronous growth change - sgc) using shaded bars. Defaults to
  `TRUE`.

- pv_alpha:

  Numeric between 0 and 1. Controls the transparency of the parallel
  variation highlight. Defaults to `0.2`.

- show_stats:

  Logical. If `TRUE`, displays crossdating statistics (e.g., overlap,
  correlation, sgc, t-values) as a plot subtitle. Defaults to `TRUE`.

- labels:

  Logical. If `TRUE`, displays year labels and points at the start and
  end positions. If `FALSE`, neither labels nor points are shown.
  Defaults to `TRUE`.

- label_size:

  Numeric. Controls the size of the year labels. Defaults to `3`.

- x_breaks:

  Numeric. The interval between x-axis tick marks (in years). If `NULL`
  (default), the interval is chosen automatically based on the length of
  the series. Must be a positive number if provided.

## Value

A `ggplot` object showing the plotted time series.

## Details

The function assumes that `x` and `y` are already calendar-dated and
aligned by row names. It performs internal trimming and optional
z-scoring before plotting. Crossdating statistics are computed using
[`trs_crossdate()`](https://ropensci.github.io/fellingdater/reference/trs_crossdate.md),
and regions of synchronous growth change (SGC) are highlighted. Required
helper functions include:
[`trs_trim`](https://ropensci.github.io/fellingdater/reference/trs_trim.md),
[`trs_zscore`](https://ropensci.github.io/fellingdater/reference/trs_zscore.md),
[`trs_end_date`](https://ropensci.github.io/fellingdater/reference/trs_end_date.md),
and
[`sgc_for_plot`](https://ropensci.github.io/fellingdater/reference/sgc_for_plot.md).
Statistics are derived from
[`trs_crossdate`](https://ropensci.github.io/fellingdater/reference/trs_crossdate.md).

Required packages: **ggplot2**, **ggtext**, **plyr**

## Examples

``` r
x <- trs_pseudo_rwl(n_series = 1, series_length = 80, end_date = 1500, prefix = "trs_")
y <- trs_pseudo_rwl(n_series = 1, series_length = 400, end_date = 1700, prefix = "ref_")
trs_plot_dated(x, y)

```
