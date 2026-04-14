# Check if input is a data.frame with one series

Validates that the input is a data.frame with exactly one column
(series). Intended for use in functions that operate on single tree-ring
series.

## Usage

``` r
check_single_series(x, arg_name = "x")
```

## Arguments

- x:

  An object to check.

- arg_name:

  A string indicating the argument name (e.g., "x" or "y") for
  informative error messages.

## Value

Invisibly returns `TRUE` if the input is valid; otherwise, throws an
error.
