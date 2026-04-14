# Create a pseudo-population of tree-ring series

Generates a synthetic tree-ring width dataset in `rwl` format, suitable
for testing crossdating and chronology construction methods.

## Usage

``` r
trs_pseudo_rwl(
  n_series = 10,
  series_length = 100,
  end_date = 2024,
  related = TRUE,
  trend = NULL,
  neg_lin_param = -0.1,
  neg_exp_param = 0.05,
  ar = TRUE,
  ar_params = 0.7,
  mean_rw = 100,
  noise_sd = 8,
  common_signal_strength = 0.75,
  prefix = "trs_",
  seed = NULL
)
```

## Arguments

- n_series:

  Integer. Number of tree-ring series to generate.

- series_length:

  Numeric or numeric vector of length 2. If a single number, all series
  have this fixed length. If a vector of two values, series lengths are
  randomly sampled between the minimum and maximum values (inclusive).

- end_date:

  Numeric or numeric vector of length 2. If a single number, all series
  end in this year. If a vector of two values, series end years are
  randomly sampled within this range.

- related:

  Logical. If `TRUE`, all series share a common growth pattern (i.e.,
  internally correlated). If `FALSE`, each series is generated
  independently.

- trend:

  Character. Type of trend to apply to series. Options: `"neg_lin"` for
  a negative linear trend, `"neg_exp"` for a negative exponential trend,
  or `NULL` for no trend.

- neg_lin_param:

  Numeric. Slope for the linear decline (typically between -0.1 and
  -1.0).

- neg_exp_param:

  Numeric. Rate parameter for the exponential decline (typically between
  0.01 and 0.1).

- ar:

  Logical. If `TRUE`, adds autocorrelation using an AR model. Note: when
  `related = TRUE`, AR is applied to the common base signal; when
  `related = FALSE`, AR is applied to each series individually.

- ar_params:

  Numeric vector. Autoregressive coefficients (e.g., `c(0.8)`, or
  `c(0.6, 0.2)`).

- mean_rw:

  Numeric. Mean ring width around which the data is centered (default is
  100).

- noise_sd:

  Numeric. Standard deviation of noise added to each series (default is
  8).

- common_signal_strength:

  Numeric. Strength of common signal when `related = TRUE` (0-1). Higher
  values produce higher inter-series correlations (rbar). Default 0.75
  typically produces rbar around 0.5-0.7, which is realistic for
  tree-ring chronologies.

- prefix:

  Character. Prefix used when naming each tree-ring series (default is
  `"trs_"`).

- seed:

  Numeric. Random seed for reproducible results. If `NULL`, no seed is
  set.

## Value

A `data.frame` of class `"rwl"`, with years as row names and each column
representing one synthetic tree-ring series.

## Examples

``` r
# Basic usage
pseudo_data <- trs_pseudo_rwl(n_series = 5, series_length = 50)

# Variable lengths and end dates with reproducible results
pseudo_data <- trs_pseudo_rwl(
  n_series = 10,
  series_length = c(80, 120),
  end_date = c(2020, 2024),
  seed = 123
)

# Related series with strong common signal (high rbar)
pseudo_data <- trs_pseudo_rwl(
  n_series = 8,
  series_length = 100,
  related = TRUE,
  common_signal_strength = 0.85, # Should produce rbar ~ 0.7-0.8
  trend = "neg_exp",
  ar = TRUE,
  ar_params = c(0.7, 0.2),
  seed = 456
)
```
