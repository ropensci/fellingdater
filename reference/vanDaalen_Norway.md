# van Daalen (unpublished) sapwood data set.

Sapwood data set for historical timbers found in the Netherlands that
were imported from Norway. Unpublished data by S. van Daalen (version 19
Dec 2022, Van Daalen Dendrochronologie - \<www.dendro.nl\>).

## Usage

``` r
vanDaalen_Norway
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

    sample size = 104 observations

## Examples

``` r
sw_data_info("vanDaalen_Norway")
#> $data
#> [1] "vanDaalen_Norway"
#> 
#> $citation
#> [1] "Sjoerd van Daalen (Van Daalen Dendrochronologie, the Netherlands), unpublished data (version 19-12-2022)"
#> 
#> $area
#> [1] "Historical timbers found in the Netherlands that were imported from Norway."
#> 
#> $n_observations
#> [1] 104
#> 
#> $summary_raw_data
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    8.00   16.00   19.00   19.64   23.00   40.00 
#> 

sw_model("vanDaalen_Norway", plot = TRUE)

```
