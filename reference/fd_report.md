# Report felling dates of individual tree-ring series

This function reports the felling date estimate of individual tree-ring
series, based on the presence/absence of sapwood and/or waney edge.
There are three possible modes of reporting:

- a *terminus post quem* or *earliest possible felling date*: when only
  heartwood rings have been observed and measured

- a felling date range or interval: when sapwood rings have been
  recorded, but no bark or waney edge is present.

- an exact felling date: when bark or waney edge is present on the
  measured sample.

## Usage

``` r
fd_report(
  x,
  series = "series",
  last = "last",
  n_sapwood = "n_sapwood",
  waneyedge = "waneyedge",
  sw_data = "Hollstein_1980",
  cred_mass = 0.954,
  densfun = "lognormal"
)
```

## Arguments

- x:

  Name of a `data.frame` with at least four columms, providing
  information on

  - the id's of the tree-ring series ("series")

  - the number of sapwood rings observed ("n_sapwood)

  - the presence of waney edge ("waneyedge")

  - the year assigned to the last measured ring ("last").

  Optionally, a column specifying the sapwood data set (`sw_data`) can
  also be included.

- series:

  Name of the column in `x` where id's of the tree-ring series are
  listed as `character` values.

- last:

  Name of the column in `x` which lists the calendar year assigned to
  the last measured ring (should be a `numeric` vector).

- n_sapwood:

  Name of the column in `x` where the number of observed sapwood rings
  are listed (should be `numeric` vector).

- waneyedge:

  Name of the column in `x` indicating the presence (`TRUE`)/absence
  (`FALSE`) of waney edge (should be a `logical` vector).

- sw_data:

  There are two options:

  - A `character` string providing the name of the sapwood data set to
    use for modelling. It should be one of the data sets listed in
    [`sw_data_overview()`](https://ropensci.github.io/fellingdater/reference/sw_data_overview.md),

  - or the name of a `data.frame` with sapwood data in columns
    `n_sapwood` and `count`, or

  - or character string naming a column in `x` that lists for each
    series the sapwood model to use, e.g. sw_data =
    "sapwood_model_column".

- cred_mass:

  A numeric `scalar [0, 1]` specifying the mass within the credible
  interval (default = .954).

- densfun:

  Name of the density function to fit to the sapwood distribution.
  Should be one of:

  - *lognormal* (the default value),

  - *normal*,

  - *weibull*,

  - *gamma*.

## Value

A `data.frame` with felling date estimates per tree-ring series. Columns
include:

- `series`: series identifier

- `last`: last measured ring

- `n_sapwood`: number of sapwood rings

- `waneyedge`: TRUE/FALSE for waney edge

- `lower`, `upper`: numeric bounds for the estimated felling date

- `felling_date`: a character summary (e.g., "between 1500 and 1510")

- `sapwood_model`: the sapwood data/model used

## See also

[`sw_interval()`](https://ropensci.github.io/fellingdater/reference/sw_interval.md),
[`sw_data_overview()`](https://ropensci.github.io/fellingdater/reference/sw_data_overview.md),
[`sw_interval_plot()`](https://ropensci.github.io/fellingdater/reference/sw_interval_plot.md)

## Examples

``` r
df <- data.frame(
     id = c("trs1", "trs2", "trs3", "trs4"),
     swr = c(7, 1, 10, 12),
     waneyedge = c(FALSE, FALSE, FALSE, TRUE),
     end = c(1482, 1475, 1490, 1498)
)
fd_report(df,
     series = "id",
     n_sapwood = "swr",
     last = "end",
     sw_data = "Wazny_1990"
)
#>   series last n_sapwood waneyedge lower upper          felling_date
#> 1   trs1 1482         7     FALSE  1483  1501 between 1483 and 1501
#> 2   trs2 1475         1     FALSE  1482  1500 between 1482 and 1500
#> 3   trs3 1490        10     FALSE  1490  1506 between 1490 and 1506
#> 4   trs4 1498        12      TRUE    NA  1498               in 1498
#>   sapwood_model
#> 1    Wazny_1990
#> 2    Wazny_1990
#> 3    Wazny_1990
#> 4    Wazny_1990

# Example with different sw_model for individual series
# You can add a user-defined sapwood dataset as well.

sapwood_model_column <- c(
     "Sohar_2012_ELL_c",
     "Wazny_1990",
     "Hollstein_1980",
     "vanDaalen_Norway"
)

df2 <- cbind(df, sw_data = sapwood_model_column)

fd_report(df2,
     series = "id",
     n_sapwood = "swr",
     last = "end",
     sw_data = "sw_data"
)
#>   series last n_sapwood waneyedge lower upper          felling_date
#> 1   trs1 1482         7     FALSE  1482  1493 between 1482 and 1493
#> 2   trs2 1475         1     FALSE  1482  1500 between 1482 and 1500
#> 3   trs3 1490        10     FALSE  1490  1514 between 1490 and 1514
#> 4   trs4 1498        12      TRUE    NA  1498               in 1498
#>      sapwood_model
#> 1 Sohar_2012_ELL_c
#> 2       Wazny_1990
#> 3   Hollstein_1980
#> 4 vanDaalen_Norway
#' @importFrom rlang .data
```
