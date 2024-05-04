---
title: "_fellingdater_: a toolkit to estimate, report and combine felling dates derived
  from historical tree-ring series."
authors:
  - name: Kristof Haneca
    orcid: 0000-0002-7719-8305
    affiliation: 1
affiliations:
  - name: Flanders Heritage Agency, Belgium
    index: 1
date: "2024-04-24"
year: 2024
output: pdf_document
tags:
  - R
  - tree-ring analysis
  - dendrochronology
  - sapwood
  - dendroarchaeology
editor_options:
  markdown:
    wrap: 72
bibliography: paper.bib
---

# Summary

Tree-ring dating, or dendrochronology, allows to assign calendar-year
dates to growth rings that can be observed on an old piece of timber.
Once a tree-ring series is securely anchored to a calendar year
time-scale, the end date of the outermost ring can be used to estimate
the year when the tree was felled.

The `fellingdater` package offers a suite of functions that can assist
dendrochronologists to infer, combine and report felling date estimates
from dated tree-ring series, based on the presence of partially
preserved sapwood or waney edge ([Fig. 1](#fig-cross-section)).

![A cross-section of a historical timber from a medieval roof
construction. All ring boundaries are marked, as well as the heartwood
and the partially preserved
sapwood.](cross-section.png){#fig-cross-section}

# Background

Dendrochronology is the most precise chronometric dating technique for
(pre-)historical wooden constructions and objects [@baillie1995]. It
involves recording the ring-width pattern on a cross-section of an
ancient wooden element and matching this pattern to absolutely dated
reference chronologies. This allows to anchor the recorded tree-ring
pattern to an absolute time scale. In archaeological, architectural or
art-historical studies, the primary objective of a dendrochronological
survey is to ascertain an accurate estimate of the **felling date** (or
dying-off) of the parent tree from which the timber originates
[@haneca2009; [@dominguez-delmas_2020]; @tegel2022]. This felling date
is the closest related and datable event to the creation of the wooden
object or construction.

The exact felling date can be inferred from the most recently formed
tree ring prior to the felling or death of the tree. Often, the wood of
the felled tree has undergone processing, trimming, or biological
deterioration leading to the irreversible loss of wood tissue. In such
cases, the timing of the felling date can only be estimated. The most
challenging situation is when neither sapwood, nor the transition
between heartwood and sapwood, remains on the object or timber ([Fig.
1](#fig-cross-section), HW/SW boundary). Then, an untraceable amount of
wood and growth layers has been removed and the last measured ring only
provides an earliest possible felling date or *terminus post quem*.

The `fellingdater` package aims to facilitate the process to infer,
combine and report felling date estimates from dated tree-ring series,
based on the presence of (partially) preserved sapwood or waney edge.

# Statement of need

Many descriptive statistics and statistical models have been published
to establish accurate estimates of the expected number of sapwood ring
[@edvardsson2022; @bleicher2020; @rybnicek2006; @pilcher1987;
@hollstein1965; @hollstein1980; @wazny1990; @miles1997; @sohar2012;
@bräthen1982; @haneca2009; @hughes1981; @jevsenak2019; @hillam1987;
@gjerdrum2013; @shindo2024]. These models often rely on
log-transformation of the original counts of sapwood numbers from living
and historical timbers, or use regression models that include additional
variables such as mean ring width, the cambial age of the tree or a
combination of both. A standardized methodology is hampered by this
variety in methodology and reporting.

A Bayesian method to improve the procedures to model sapwood data,
compute lower and upper limits for the felling date based upon the
selected sapwood model and a given credible interval have been
introduced by @millard2002a. This procedure was then further refined by
@miles2006, and critically reviewed with real-life examples by
@tyers2008. This workflow has been incorporated in
[OxCal](https://c14.arch.ox.ac.uk/oxcalhelp/Sapwood.html), the routine
software for radiocarbon dating and modelling [@bronkramsey2009].
Tree-ring analysis, on the other hand, relies on a growing set of
R-packages, with the '*Dendrochronology Program Library in R*'
[@bunn2008; @bunn2010; @bunn2022], at it's core (
[opendendro.org](https://opendendro.org/); [@bunn2022a]). Yet, the
reporting of felling dates is currently not a standardized procedure
incorporated in an R-package.

The `fellingdater`package allows to fully document the methodology to
establish a felling date -- for a single timber or a group of timbers --
making the whole procedure reproducible and assists in building
standardized workflows when applied to large datasets (e.g.
[@haneca2020]). The package includes functions related to each step in
the (generalized) workflow when analysing historical tree-ring series
([Fig. 2](#fig-workflow)).

![A generalized workflow and related functions, for inferring felling
dates from tree-ring dated historical
timbers.](workflow.png){#fig-workflow}

# Data within the package

The package comes with published datasets of sapwood counts, retrieved
from their original publication [@haneca2012]. This was only possible
for a limited number of datasets as many have been published as
histograms with wide bins (\>1), what does do not allow to retrieve the
underlying data points. An overview of all included sapwood datasets is
generated by `sw_data_overview()`.

More information on the datasets, such as the bibliographic reference to
the original publication, the wood species and some basic descriptive
statistics can be accessed by `sw_data_info(<name_of_dataset>).`

`sw_model()` fits a density distribution to the original data, and
returns the output of the modelling process. With `sw_model_plot()` the
model is visualized as a ggplot-style graph [@wickham2016] ([Fig.
3](#fig-sw-model)).

``` r
library(fellingdater)

sw_data_overview()
#>  [1] "Brathen_1982"     "Hollstein_1980"   "Miles_1997_NM"    "Miles_1997_SC"   
#>  [5] "Miles_1997_WBC"   "Pilcher_1987"     "Sohar_2012_ELL_c" "Sohar_2012_ELL_t"
#>  [9] "Sohar_2012_FWE_c" "Sohar_2012_FWE_t" "Wazny_1990"       "vanDaalen_NLBE"  
#> [13] "vanDaalen_Norway"

model <- sw_model("Hollstein_1980", plot = FALSE)
sw_model_plot(model)
```

![Two sapwood datasets with a density function modeled to the raw
data.](sw_model_plot.jpg){#fig-sw-model}

# Example of use

## Installation

The latest version is hosted on
[GitHub](https://github.com/ropensci/fellingdater/) and
[R-universe](https://ropensci.r-universe.dev/fellingdater).

``` r
pak::pak("ropensci/fellingdater")

# or

install.packages("fellingdater", repos = "https://ropensci.r-universe.dev")
```

## Reading tree-ring files

The function `read_fh()` is an extension to the `dplR::read.fh()`
function and allows to read .fh ([format
Heidelberg](https://www.treeringsociety.org/resources/SOM/Brewer_Murphy_SupplementaryMaterial.pdf))
files of ring widths (both in decadal, half-chrono and chrono format)
[@brewer2011]. The function is focused on retrieving information found
in the HEADER fields of the .fh files, that often harbour essential
information necessary to establish a well informed estimate of the
felling date. The `read_fh()` function retrieves the information from
the HEADER fields and lists the items as attributes to the ring-width
measurements. The `fh_header()`function facilitates easy conversion to a
`data.frame`.

## Crossdating

The function `cor_table()` computes commonly used correlation values
between dated tree-ring series and reference chronologies. This function
helps to check the assigned end date of the series by comparing the
measurements against absolutely dated reference chronologies, and
thereby provide information on timber provenance. The latter allows to
select the most appropriate sapwood model for the tree-ring data.

The correlation values computed are:

-   glk: 'Gleichläufigkeit' or 'percentage of parallel variation'
    [@buras2015; @eckstein1969; @huber1943; @visser2021].

-   glk_p: significance level associated with the glk-value
    [@jansma1995].

-   r_pearson: the Pearson's correlation coefficient

-   t_St: Student's t-value based on r_pearson

-   t_BP: t-values according to the @baillie1973 algorithm

-   t_Ho: t-values according to the @hollstein1980 algorithm

``` r
Doel1_trs <- read_fh(Doel1, header = FALSE)
Hollstein_crn <- read_fh("Hollstein80.fh", header = FALSE)

cor_table(x = Doel1_trs,
          y = Hollstein_crn,
          min_overlap = 80, 
          output = "table",
          sort_by = "t_BP") 
```

## Felling date interval

After selecting the appropriate sapwood model (e.g. , one of [Fig.
2](#fig-sw-model)) one can use the model to estimate the upper and lower
limits of the number of missing sapwood rings. The function
`sw_interval()` calculates the probability density function (PDF) and
highest probability density interval (HDI) of the felling date range
based on the observed number of sapwood rings (`n_sapwood = ...`), their
chronological dating (`last = ...`) and the selected sapwood data
(`sw_data = ...`) and model (`densfun = ...`).

In the example below, 10 sapwood rings were observed on a historical
timber (last ring dated to 1234 CE) that is supposed to have a
provenance in the Southern Baltic region (sapwood model published by
[@wazny1990]). The HDI delineates an interval in which the actual
felling date is most likely situated ([Fig. 4](#fig-sw_interval)).

Note that the more sapwood rings that have been measured, the more
probability mass is assigned to the tails of the sapwood model.

``` r
# 10 sapwood rings observed and the Wazny 1990 sapwood model:

interval <- sw_interval(
     n_sapwood = 10, 
     last = 1234, 
     hdi = TRUE, 
     cred_mass = .95, 
     sw_data = "Wazny_1990", 
     densfun = "lognormal", 
     plot = TRUE)
```

![A truncated lognormal distribution, representing the sapwood model for
a tree-ring series with 10 sapwood rings. The black line delineates the
95% credible interval for the felling
date.](sw_interval.jpg){#fig-sw-interval align="center" width="60%"}

## Combine felling dates

### sw_combine

The procedure to combine felling dates of a group of related tree-ring
series with (partially) preserved sapwood, in order to narrow down the
range of a common felling date, is provided by the function
`sw_combine()` . This function returns a `list` with:

-   the probability density function (PDF) for the felling date of the
    individual series and the PDF of the model that combines these
    individual series (`$data_raw`),

-   the HDI for the combined estimate of the common felling date
    (`$hdi_model`),

-   the *Agreement index* (`$A_model`) of the model, expressing how well
    the individual series fit into the model ,

-   an overview of the felling date range for the individual series
    (`$individual_series`), and their *Agreement index* (*A~i~*) to the
    combined model.

The function `sw_combine_plot()` allows to visualize the output.

The rationale and mathematical background of the *Agreement index*
(*A~i~*)was introduced and developed by Bronk Ramsey [-@bronkramsey1995;
-@bronkramseymethods2017]. Both the *A~i~* of the individual series as
for the whole model (*A~model~*) should ideally be around 100%, and not
lower than the critical threshold *A~c~* = 60%.

The example dataset below consists of 5 dated tree-ring series of which
one has an exact felling date ([Fig. 5, left](#fig-sw-combine)). The
proposed combined felling date equals the felling date of the series
with an exact felling date (trs_15), but now it can be assessed that
this falls within the felling date ranges for three other individual
series (trs_11, trs_12 and trs_14). One other series (trs_13) has no
remaining sapwood and therefore only an earliest possible felling date
can be given (arrow pointing away from last measured ring). The
agreement indexes of all individual series and the overall model are
high and above the critical threshold of 60%.

``` r
trs_example2
#>   series last n_sapwood waneyedge
#> 1 trs_11 1000         5     FALSE
#> 2 trs_12 1005        10     FALSE
#> 3 trs_13 1008        NA     FALSE
#> 4 trs_14 1000         1     FALSE
#> 5 trs_15 1010         3      TRUE

p1 <- sw_combine(trs_example2, plot = TRUE)
```

![Graphical output of `sw_combine()`. The sapwood model for the
individual series in light grey, the probability density function of the
combined felling in dark grey tone. The credible interval for the
felling date of individual series is shown as a dashed red line and a
black line for the combined estimate. The dataset in the left graph
includes an exact felling date that matches with the estimates for the
other series. The graph on the right shows a model that fails to group
all series around a common felling
date.](sw_combine.jpg){#fig-sw-combine}

In the next example, an attempt to compute a common felling date for a
group of 5 tree-ring series fails. All but one of the series include
partially preserved sapwood, but these tree-ring series do not share a
common timing for their estimated felling date ([Fig. 5,
right](#fig-sw-combine)). The agreement index of the model is far below
60%, as is the case for most of the individual series.

``` r
trs_example4
#>   series last n_sapwood waneyedge
#> 1 trs_21 1000         5     FALSE
#> 2 trs_22 1005        10     FALSE
#> 3 trs_23 1005        NA     FALSE
#> 4 trs_24 1020         1     FALSE
#> 5 trs_25 1040         0     FALSE

p2 <- sw_combine(trs_example4, plot = TRUE)
```

## Sum felling dates

For large datasets of dated tree-ring series, it is not always
straightforward to assess temporal trends in the frequency of felling
dates. The individual series each have their own probability density
function based on a chosen sapwood model and the number of observed
sapwood rings. To make another reference to radiocarbon dating, it is
common practice in the analysis of large volumes of calibrated
radiocarbon dates to compute the *summed probability densities* (SPD) of
the calibrated radiocarbon dates. Summed probabilities are used to
determine the temporal density of ages (events), where there is no clear
prior information on their distribution [@bronkramseymethods2017]. This
procedure is implemented in OxCal and the R-package `rcarbon`
[@crema2020]. The function `sw_sum()` makes his procedure available for
tree-ring analyses. The summed probability distribution (SPD) of the
individual probability densities of felling dates of single tree-ring
series with incomplete sapwood allows to visualize fluctuations in the
incidence of potential felling dates through time. Exact felling dates
derived from tree-ring series with waney edge are not included in the
computational process of the SPD as they would result in anomalous
spikes in the SPD, as their associated probability (*p* = 1) would be
assigned to a single calendar year. Therefore exact felling dates are
plotted separately on top of the SPD ([Fig. 6](#fig-sw_sum)).

``` r
sum <- sw_sum(trs_example7)

sw_sum_plot(sum, dot_size = 2, dot_shape = 25)
```

![Graphical representation of the out put of `sw_sum()`. The blue bars
represent the summed probability density (SPD) of the individual series
with partial sapwood. The red line is a rectangular filter applied to
the SPD to highlight the general trend. Series with exact felling dates
are plotted as triangles.](sw_sum.jpg){#fig-sw-sum}

# Future work

In its current version the package `fellingdater` is tailored to the
general workflow for analyzing tree-ring datasets from wooden cultural
heritage objects and constructions, made of European oak (*Quercus*
sp.). The sapwood data included in the current version reflect this
focus on oak. But all functions can also work with a custom sapwood
dataset, provided as a `data.frame`. As such, sapwood data from other
regions and wood species can also be explored, modeled and used to
report felling dates.

# Acknowledgements

Koen Van Daele and Ronald Visser fueled me with valuable feedback on
earlier versions of the package.

At rOpenSci, dr. Antonio J. Pérez-Luque, dr. Nicholas Tierney and dr.
Maëlle Salmon provided an essential and constructive software review,
allowing me to significantly improve the quality of the package.

# References
