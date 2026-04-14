# Compute a running mean on a time series

This vectorized function computes a running mean/moving average over a
continuous (time) series with a specified window width.

## Usage

``` r
mov_av(x, w = 11, align = "center", edges = "fill")
```

## Arguments

- x:

  A `numeric` vector of `length(x)`, with the time series data.

- w:

  The width of the moving average window. When `w`is even, one more
  value from the future is included.

- align:

  The alignment of the window relative to the current data point. It can
  be one of:

  - `align = "center"`: The average is assigned to the center of the
    window (default).

  - `align = "left"`: The average includes the current value and the
    next (w-1) values.

  - `align = "right"`: The average includes the current value and the
    previous (w-1) values.

- edges:

  Defines how values are computed at the start and end of the series.
  Options are:

  - `edges = "fill"`: The average is calculated with a decreasing number
    of values near the edges of the vector (default).

  - `edges = "nofill"`: The result includes `NA` values when the window
    does not cover w values.

## Value

A `numeric` vector of the same length of `x` with the computed running
mean values.

## Examples

``` r
num_vec <- sample(seq(50, 100, 1), 100, replace = TRUE)
filtered <- mov_av(num_vec, w = 5, align = "center", edges = "nofill")
plot(num_vec, type = "l")
lines(filtered, col = "darkblue")

```
