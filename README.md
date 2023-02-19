
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `fellingdateR`: estimating felling dates from historical tree-ring series

<!-- badges: start -->
<!-- badges: end -->

This R-package offers a set of functions that will help you to infer
felling date estimates from dated tree-ring series. The presence of
(partially) preserved sapwood or waney edge allows to estimate a range
for the actual felling date, for individual series as well as for a
group of timbers. Furthermore, an additional function provides a tool to
sum sapwood probability distributions, comparable to ‘summed probability
densities’ commonly applied to sets of radiocarbon (<sup>14</sup>C)
dates.

Where it can be assumed that a group of historical timbers were all
felled at the same time (i.e. the same year), but due to the absence of
the bark/cambial zone (waney edge) and the last formed tree ring this
cannot be assessed, the preserved sapwood rings can be used to infer a
date range for the felling date. Taking into account the observed number
of sapwood rings on all samples and combining them into a single
estimate, is likely to provide a more accurate and precise estimate of
the felling date year for the group of timbers under study. It is
assumed that this estimate of the felling date is closely related to the
construction date of the timber structure or building phase that was
sampled for tree-ring analysis and dating.

## Installation

You can install the development version of fellingddateR from
[GitHub](https://github.com/) with:

``` r
#install.packages("devtools")
devtools::install_github("hanecakr/fellingdateR")
```

## Examples

The following example shows a felling date estimate for a set of four
dated tree-ring series:

``` r
## a dataset where all series have partially preserved sapwood
dummy1 <- data.frame(
  series = c("trs_1", "trs_2", "trs_3", "trs_4", "trs_5"),
  last = c(1000, 1009, 1007, 1005, 1010),
  n_sapwood = c(5, 10, 15, 16, 8),
  waneyedge = c(FALSE, FALSE, FALSE, FALSE, FALSE))
dummy1
#>   series last n_sapwood waneyedge
#> 1  trs_1 1000         5     FALSE
#> 2  trs_2 1009        10     FALSE
#> 3  trs_3 1007        15     FALSE
#> 4  trs_4 1005        16     FALSE
#> 5  trs_5 1010         8     FALSE
```

``` r

library(fellingdateR)
# basic example

sw_combine(dummy1, plot = TRUE)
#> Warning: Using one column matrices in `filter()` was deprecated in dplyr 1.1.0.
#> ℹ Please use one dimensional logical vectors instead.
#> ℹ The deprecated feature was likely used in the dplyr package.
#>   Please report the issue at <]8;;https://github.com/tidyverse/dplyr/issueshttps://github.com/tidyverse/dplyr/issues]8;;>.
```

<img src="man/figures/README-example-1.png" width="100%" />

The sapwood data used in the example below was published by Hollstein in
1980:

``` r

sw_model("Hollstein_1980")
```

<img src="man/figures/README-model_sapwood_counts-1.png" width="100%" />

## Motivation

This package was developed during the analysis of a large data set of
tree-ring series that originate from medieval timber constructions in
the town of [Bruges](https://en.wikipedia.org/wiki/Bruges) (Belgium).
The results of this study are presented in a paper published in
[*Dendrochronologia*](https://www.journals.elsevier.com/dendrochronologia).

> Kristof HANECA
> [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-7719-8305),
> Vincent DEBONNE, Patrick HOFFSUMMER 2020. The ups and downs of the
> building trade in a medieval city: tree-ring data as proxies for
> economic, social and demographic dynamics in Bruges (*c.* 1200 –
> 1500). *Dendrochronologia* 64, 125773.  
> <https://doi.org/10.1016/j.dendro.2020.125773>

## Main functions

### sw_interval

This function computes the probability density function (PDF) and
highest probability density interval (hdi) of the felling date range
baased on the observed number of sapwood rings, their chronological
dating and the selected sapwood data and model.

``` r
# The following code computes the PDF of sapwood estimates, or limits of the hdi for the felling date.
# In the example below, 10 sapwood rings were observed on a sample (last ring dated to 1234 AD) that is supposed to have a provenance in the Southern Baltic  region (sapwood model published by Wazny, 1990).
# The hdi delineates an interval in which the actual felling date is most likely situated. It is the shortest interval within a probability distribution for a given probability mass or credible interval. The hdi summarizes the distribution by specifying an interval that spans most of the distribution, say 95% of it, as such that every point inside the interval has higher credibility than any point outside the interval.

sw_interval(n_sapwood = 10,
            last = 1234,
            hdi = TRUE,
            credMass = .95, 
            sw_data = "Wazny_1990", 
            densfun = "lognormal")
#>   lower upper         p
#> 1  1234  1250 0.9611797
```

When `hdi = FALSE` a matrix is returned with scaled p values for each
number of observed sapwood rings.

``` r
# 8 sapwood rings observed and the Hollstein 1980 sapwood model:
pdf <- sw_interval(n_sapwood = 10,
            last = 1234,
            hdi = FALSE,
            credMass = .95, 
            sw_data = "Wazny_1990", 
            densfun = "lognormal")

full <- sw_interval(n_sapwood = 0,
            last = 1224,
            hdi = FALSE,
            credMass = .95, 
            sw_data = "Wazny_1990", 
            densfun = "lognormal")

# plot the probability distribution of the 'truncated' sapwood model:
ggplot2::ggplot(pdf) +
  ggplot2::geom_area(ggplot2::aes(x = year, y = p), fill = "burlywood2", color = "burlywood4") +
  ggplot2::theme_minimal()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

### fd_report

### sw_combine and sw_combine_plot

## Helper functions

### sw_data_overview

The function `sw_data_overview` provides an overview of all published
sapwood data sets that are distributed with this package.

``` r

sw_data_overview()
#>  [1] "Brathen_1982"     "Hollstein_1980"   "Miles_1997_NM"    "Miles_1997_SC"   
#>  [5] "Miles_1997_WBC"   "Pilcher_1987"     "Sohar_2012_ELL_c" "Sohar_2012_ELL_t"
#>  [9] "Sohar_2012_FWE_c" "Sohar_2012_FWE_t" "Wazny_1990"       "vanDaalen_NLBE"  
#> [13] "vanDaalen_Norway"
```

### sw_data_info

More information on one of the sawpood data sets - how to cite, the area
they represent, etc. - can be retrieved by the `sw_data_info` function.

``` r

sw_data_info("Pilcher_1987")
#> $data
#> [1] "Pilcher_1987"
#> 
#> $citation
#> [1] "Pilcher J.R. 1987. A 700 year dating chronology for northern France. Applications of tree-ring studies. Current research in dendrochronology and related subjects. BAR International Series 333, 127–139."
#> 
#> $area
#> [1] "Northern France"
#> 
#> $n_observations
#> [1] 116
#> 
#> $summary_raw_data
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   12.00   22.00   26.00   26.72   31.00   49.00
```

### sw_model

A graphical representation of the sapwood data sets is provided by the
`sw_model` function.

``` r

sw_model("Sohar_2012_ELL_c")
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

``` r

sw_data_info("Sohar_2012_ELL_c")
#> $data
#> [1] "Sohar_2012_ELL_c"
#> 
#> $citation
#> [1] "Sohar K., Vitas A. & Läänelaid A. 2012. Sapwood estimates of pedunculate oak (Quercus robur L.) in eastern Baltic, Dendrochronologia 30.1, 49–56. DOI: https://doi.org/10.1016/j.dendro.2011.08.001"
#> 
#> $area
#> [1] "Eastern Estonia, Latvia, Lithuania (sapwood determined by color)."
#> 
#> $n_observations
#> [1] 562
#> 
#> $summary_raw_data
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    2.00   10.00   12.00   11.69   13.00   27.00
```

This function allows to fit a distribution to a data set of observed
sapwood numbers and computes the highest posterior density interval
(hdi) for a given credibility mass. The density function fitted to the
sapwood data set should be one of:

-   “lognormal” (the default value),
-   “normal”,
-   “weibull”,
-   “gammma”.

The credible interval should be a value between 0 and 1.

``` r

sw_model("Wazny_1990", densfun = "weibull", credMass= .90, plot = TRUE)
#> Warning in densfun(x, parm[1], parm[2], ...): NaNs produced
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

When `plot = FALSE`, a list with the numeric output of the modelling
process is returned.

### read_fh and get_header
