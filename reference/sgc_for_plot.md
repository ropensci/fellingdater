# Detect synchronous growth changes between two tree-ring series ( helper for trs_plot_dated() )

Identifies years when two tree-ring series show synchronous growth
changes (SGC)

- both increasing or both decreasing from the previous year. Based on
  the logic used in trs_pv() for distinguishing between synchronous and
  semi-synchronous growth.

## Usage

``` r
sgc_for_plot(x, y)
```

## Arguments

- x:

  A `data.frame` with one column of ring-width values and years as row
  names.

- y:

  A `data.frame` with one column of ring-width values and years as row
  names.

## Value

A `data.frame` with two columns:

- pv_logi:

  Logical vector indicating synchronous growth changes (TRUE) or not
  (FALSE)

- year:

  Numeric vector of years corresponding to the logical values

## Details

The function compares year-to-year growth direction changes between two
series using the same logic as trs_pv():

- Synchronous Growth Changes (SGC): Both series change in the same
  direction (both + or both -)

- Semi-synchronous/Asynchronous: One series changes while other doesn't,
  or they change in opposite directions

Only years with synchronous growth changes receive TRUE values. Years
with missing values in either series are marked as FALSE. The first year
cannot be evaluated (no previous year for comparison) and is marked as
FALSE.

## Examples

``` r
# Create sample data
years <- 1950:1980
x <- data.frame(series1 = runif(length(years), 0.5, 2.0))
rownames(x) <- years
y <- data.frame(series2 = runif(length(years), 0.8, 1.8))
rownames(y) <- years

# Detect synchronous growth changes
sync_growth <- sgc_for_plot(x, y)
head(sync_growth)
#>      sgc_logi ssgc_logi year
#> 1950     TRUE     FALSE 1950
#> 1951     TRUE     FALSE 1951
#> 1952     TRUE     FALSE 1952
#> 1953     TRUE     FALSE 1953
#> 1954     TRUE     FALSE 1954
#> 1955    FALSE     FALSE 1955
```
