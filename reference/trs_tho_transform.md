# Pre-transform tree-ring series using Hollstein method

Computes log-transformed growth ratios for tree-ring series following
the Hollstein (1980) method: `100 * log10(value_t / value_(t-1))`. This
transformation emphasizes year-to-year growth variations.

## Usage

``` r
trs_tho_transform(x)
```

## Arguments

- x:

  A rwl-style data frame with years as rownames and series as columns.
  All columns must be numeric.

## Value

A matrix of log-transformed growth ratios with the same dimensions as
the input. The first row (first year) is set to NA since no previous
year exists for ratio calculation. Row names match the input data.

## Details

The Hollstein transformation computes year-to-year growth ratios:

\$\$G_t = 100 \times \log\_{10}(x_t / x\_{t-1})\$\$

where \\x_t\\ is the ring width at year \\t\\. This transformation:

- Emphasizes relative changes between consecutive years

- Reduces the influence of long-term trends

- Makes series more suitable for crossdating analysis

Values that would result in undefined logarithms (zero or negative
ratios) are set to NA. The first year of each series is always NA since
no previous year exists for ratio calculation.

## References

Hollstein, E. (1980). *Mitteleuropäische Eichenchronologie*. Verlag
Philipp von Zabern, Mainz.

## See also

[`trs_tbp_transform`](https://ropensci.github.io/fellingdater/reference/trs_tbp_transform.md)
for Baillie & Pilcher transformation,
[`trs_tho`](https://ropensci.github.io/fellingdater/reference/trs_tho.md)
for computing Hollstein t-values

## Examples

``` r
if (FALSE) { # \dontrun{
# Create sample rwl data
rwl_data <- trs_pseudo_rwl(n_series = 3, series_length = 50, end_date = 2020)

# Apply Hollstein transformation
transformed <- trs_tho_transform(rwl_data)

# Check dimensions (should be one row fewer)
nrow(rwl_data) # Original rows
nrow(transformed) # One fewer row

# View first few transformed values
head(transformed)
} # }
```
