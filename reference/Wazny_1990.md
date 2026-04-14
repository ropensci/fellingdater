# Wazny 1990 sapwood data set.

Sapwood data set for Poland, published by Wazny in 1990.

## Usage

``` r
Wazny_1990
```

## Format

A tibble of 2 variables:

- n_sapwood:

  number of sapwood rings

- count:

  number of times n_sapwood was observed

## Source

Wazny T. 1990. Aufbau und Anwendung der Dendrochronologie für Eichenholz
in Polen (PhD dissertation). Hamburg University, Hamburg.

## Details

    sample size = 206 observations

## Examples

``` r
sw_data_info("Wazny_1990")
#> $data
#> [1] "Wazny_1990"
#> 
#> $citation
#> [1] "Wazny T. 1990. Aufbau und Anwendung der Dendrochronologie für Eichenholz in Polen (PhD dissertation). Hamburg University, Hamburg."
#> 
#> $area
#> [1] "Poland"
#> 
#> $n_observations
#> [1] 206
#> 
#> $summary_raw_data
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    6.00   13.00   16.00   16.35   20.00   31.00 
#> 

sw_model("Wazny_1990", plot = TRUE)

```
