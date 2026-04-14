# Pre-transform tree-ring series using Baillie & Pilcher method

Applies a 5-year centered moving average and computes the log index:
`log(100 * value / MA5)`. Uses edge mirroring to maintain constant
window size and avoid NA values.

## Usage

``` r
trs_tbp_transform(x)
```

## Arguments

- x:

  A rwl-style data frame.

## Value

A matrix of log-transformed index values.

## Examples

``` r
# Create sample data,
trs <- trs_pseudo_rwl(n_series = 5, series_length = 100, end_date = c(2020, 2025))
transformed <- trs_tbp_transform(trs)
head(transformed, 10)
#>         trs_1    trs_2    trs_3    trs_4    trs_5
#> 1921       NA 4.563497       NA       NA       NA
#> 1922       NA 4.593122 4.526858       NA       NA
#> 1923       NA 4.587152 4.595120       NA       NA
#> 1924       NA 4.670922 4.676149       NA       NA
#> 1925       NA 4.570485 4.603249 4.579610       NA
#> 1926 4.573622 4.553210 4.563339 4.579558 4.554686
#> 1927 4.537781 4.576999 4.561287 4.575971 4.511172
#> 1928 4.621086 4.648010 4.601698 4.650238 4.632663
#> 1929 4.688198 4.708028 4.686881 4.663009 4.724090
#> 1930 4.619059 4.600183 4.598634 4.555951 4.581520
```
