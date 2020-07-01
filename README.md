
-----

title: “fellingDateR: brief overview” author: “Kristof Haneca” date: “01
juli, 2020” output: github\_document

-----

# Estimating felling dates from historical tree-ring series

The set of functions presented on this Github repository will help you
infer felling date estimates from dated tree-ring series with partially
preserved sapwood. Furthermore an additional function provides a tool to
sum sapwood probability distributions, comparable to ‘summed probability
densities’ commonly used for radiocarbon (<sup>14</sup>C) age
determinations.

Where it can be assumed that a group of historical timbers were all
felled at the same time (i.e. the same year), but due to the absence of
bark/cambial zone (waney edge) and the last formed tree ring this cannot
be assessed, the preserved sapwood rings on core samples can be used to
infer a date range for the felling date. Taking into account the sapwood
numbers on all samples and combining them into a single estimate, is
likely to provide a more accurate and precise estimate of the feeling
date year for the group of timber under study. It is assumed that this
estimate of the felling date is closely related to the construction date
of the structure or building phase that was sampled for tree-ring
dating.

## Motivation

These R scripts and functions were developped during the analysis of a
large set of tree-ring data that were taken from medieval timber
constructions in the town of Bruges (Belgium). The results of this study
are presented in a paper that was submitted to *Dendrochronologia* and
is currently under peer review.

> Haneca, Kristof
> [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-7719-8305),
> Debonne, Vincent
> [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0003-4536-146X),
> Hoffsummer, Patrick, (under review). The ups and downs of the building
> trade in a medieval city: tree-ring data as proxies for economic,
> social and demographic dynamics in Bruges (c. 1200 – 1500).
> *Dendrochronologia*.

## Overview of R-functions

### sapwood\_PDF

The function `sapwood_PDF()` computes the probability density function
(PDF) for the estimated felling dated, derived from the number of
observed sapwood rings, on a e.g. a core sample of cross section, and a
chosen sapwood model that gives the probability for any number of
sapwood rings (as observed on samples with a complete pith to bark
sequence).

The examples below all rely on published sapwood models for European oak
( *Quercus robur* L. and *Quercus petraea* (Matt.) Liebl.).

The `sapwood_PDF()`-function takes 5 arguments: - swr = the observed
number of sapwood rings on a timber - last = a calendar date for the
last measured tree ring on a dendrochronologically dated sample
(optional) - model = should be one of `c("Holstein_1980", "Wazny_1990")`
(more models will be added later) - hdi = `TRUE/FALSE` whether the
highest probability density interval (hdi) should be computed or not
(relies on package `HDInterval`) - credMass = number \[0, 1\] that
assigns the credibility mass associated with the hdi

OUtput is a `data.frame` with 3 variables: - `year`: ascending sequence
staring at 0 when last is not set to a calendar year, or starting from
the calendar year of the last observed sapwood ring - `swr`: ascending
sequence starting at the observed number of sapwood rings - `p`:
probability associated with the number of sapwood rings (swr), based on
the sapwood model provided

``` r
source("./R/sapwood_PDF.R")
library(tidyverse)
#> -- Attaching packages ----------------------------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --
#> v ggplot2 3.3.0     v purrr   0.3.3
#> v tibble  3.0.0     v dplyr   0.8.5
#> v tidyr   1.0.2     v stringr 1.4.0
#> v readr   1.3.1     v forcats 0.5.0
#> -- Conflicts -------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()

# 8 sapwood rings observed and the Hollstein 1980 model as a reference
sw1 <- sapwood_PDF(swr = 8, last = 1234, model = "Hollstein_1980")
ggplot(sw1) +
  geom_area(aes(x = year, y = p), fill = "burlywood2", color = "burlywood4") +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

The hdi delineates an interval in which the actual felling date is most
likely situated. It is the shortest interval within a probability
distribution for a given probability mass or credible interval. The hdi
summarizes the distribution by specifying an interval that spans most of
the distribution, say 95% of it, such that every point inside the
interval has higher credibility than any point outside the interval.

In the example below, 10 sapwood rings were observed on a sample (last
ring dated to 1234 AD) that is supposed to have a provenance in the
Southern Baltic region (sapwood model published by Wazny, 1990). The
full sampwood model is shown with a black outline. The colored part of
the distribution shows the truncated distribution at 10 observed sawpood
rings and the horizontal line depicts the 95.4% credible interval for
the felling date of the tree.

``` r
library(HDInterval) # this package assist to compute the highest density interval

# the 'full' sapwood model (Wazny 1990)
sw2 <- sapwood_PDF(swr = 0, last = 1224, model = "Wazny_1990")

# the 'truncated' sapwood model when 7 sapwood rings are observed
sw3 <- sapwood_PDF(swr = 10, last = 1234, model = "Wazny_1990")

# the highest probability density interval (hdi), with a credible interval of 95.4%
sw4 <- sapwood_PDF(swr = 10, last = 1234, hdi = TRUE, credMass = 0.954, model = "Wazny_1990")
sw4
#> lower upper 
#>  1234  1250 
#> attr(,"credMass")
#> [1] 0.954
#> attr(,"sapwood_model")
#> [1] "Wazny_1990"
```

``` r

ggplot(sw2) +
  geom_area(aes(x = year, y = p), fill = NA, color = "black") +
  geom_area(data = sw3, aes(x = year, y = p), fill = "burlywood2", color = "burlywood4") +
  geom_segment(aes(y = .005, yend = .005, x = sw4["lower"], xend = sw4["upper"])) +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
