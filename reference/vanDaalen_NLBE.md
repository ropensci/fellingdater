# van Daalen (unpublished) sapwood data set.

Sapwood data set for historical timbers found in the Netherlands en
Belgium, with a local provenance. Unpublished data by S. van Daalen
(version 19 Dec 2022, Van Daalen Dendrochronologie - \<www.dendro.nl\>).

## Usage

``` r
vanDaalen_NLBE
```

## Format

A tibble of 2 variables:

- n_sapwood:

  number of sapwood rings

- count:

  number of times n_sapwood was observed

## Source

S. van Daalen, unpublished dataset (version: 19 Dec 2022).

## Details

    sample size = 644 observations

## Examples

``` r
sw_data_info("vanDaalen_NLBE")
#> $data
#> [1] "vanDaalen_NLBE"
#> 
#> $citation
#> [1] "Sjoerd van Daalen (Van Daalen Dendrochronologie, the Netherlands), unpublished data"
#> 
#> $area
#> [1] "Historical timbers found in the Netherlands en Belgium, with a local provenance."
#> 
#> $n_observations
#> [1] 644
#> 
#> $summary_raw_data
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    4.00   15.00   19.00   19.67   23.00   54.00 
#> 

sw_model("vanDaalen_NLBE", plot = TRUE)

```
