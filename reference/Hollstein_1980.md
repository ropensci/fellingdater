# Hollstein 1980 sapwood data set.

Sapwood data set for South and Central Germany published by Hollstein in
1980.

## Usage

``` r
Hollstein_1980
```

## Format

A tibble of 2 variables:

- n_sapwood:

  number of sapwood rings

- count:

  number of times n_sapwood was observed

## Source

Hollstein E. 1980. Mitteleuropäische Eichenchronologie: Trierer
dendrochronologische Forschungen zur Archäologie und Kunstgeschichte,
Trierer Grabungen und Forschungen. Verlag Phillipp von Zabern, Mainz am
Rhein.

## Details

    sample size = 490 observations

## Examples

``` r
sw_data_info("Hollstein_1980")
#> $data
#> [1] "Hollstein_1980"
#> 
#> $citation
#> [1] "Hollstein E. 1980. Mitteleuropäische Eichenchronologie: Trierer dendrochronologische Forschungen zur Archäologie und Kunstgeschichte, Trierer Grabungen und Forschungen. Verlag Phillipp von Zabern, Mainz am Rhein.\n\nDigitized from the originale publication by: Haneca K., Debonne V., 2012. Precise tree-ring dating of building activities despite the absence of bark: A case-study on medieval church roofs in Damme, Belgium. Dendrochronologia 30, 23–34. https://doi.org/10.1016/j.dendro.2011.06.002"
#> 
#> $area
#> [1] "South and Central Germany"
#> 
#> $n_observations
#> [1] 490
#> 
#> $summary_raw_data
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    6.00   13.00   17.00   18.16   22.00   67.00 
#> 

sw_model("Hollstein_1980", plot = TRUE)

```
