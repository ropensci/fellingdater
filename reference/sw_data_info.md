# Detailed information on a sapwood data set

This function returns more information on a sapwdood data set. It lists
the correct citation for the data set, the area the data set is supposed
to represent, the number of observations and summary stats of the raw
data.

## Usage

``` r
sw_data_info(x = NULL)
```

## Arguments

- x:

  The name of a sapwood data set. you can use
  [`sw_data_overview()`](https://ropensci.github.io/fellingdater/reference/sw_data_overview.md)
  to get an overview of available data sets.

## Value

A `list` with the following components:

- `data`: the name of the data set,

- `citation`: the correct citation for the data set,

- `area`: the area represented by the data set,

- `n_observations`: the number of observations in the data set and,

- `summary_raw_data`: summary stats of the raw data.

## Examples

``` r
# Get detailed information on a sapwood data set
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

# Retrieve the citation for a specific data set
sw_data_info("Sohar_2012_FWE_c")$citation
#> [1] "Sohar K., Vitas A. & Läänelaid A. 2012. Sapwood estimates of pedunculate oak (Quercus robur L.) in eastern Baltic, Dendrochronologia 30.1, 49–56. DOI: https://doi.org/10.1016/j.dendro.2011.08.001"
```
