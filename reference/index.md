# Package index

## Sapwood functions

- [`sw_model()`](https://ropensci.github.io/fellingdater/reference/sw_model.md)
  : Model sapwood data and compute the highest posterior density
  interval

- [`sw_model_plot()`](https://ropensci.github.io/fellingdater/reference/sw_model_plot.md)
  :

  Plot function for the output of
  [`sw_model()`](https://ropensci.github.io/fellingdater/reference/sw_model.md)

- [`sw_interval()`](https://ropensci.github.io/fellingdater/reference/sw_interval.md)
  : Computes the limits of the felling date range

- [`sw_interval_plot()`](https://ropensci.github.io/fellingdater/reference/sw_interval_plot.md)
  :

  Plot function for the output of
  [`sw_interval()`](https://ropensci.github.io/fellingdater/reference/sw_interval.md)

- [`fd_report()`](https://ropensci.github.io/fellingdater/reference/fd_report.md)
  : Report felling dates of individual tree-ring series

- [`sw_combine()`](https://ropensci.github.io/fellingdater/reference/sw_combine.md)
  : Combine multiple sapwood estimates into a single felling date range

- [`sw_combine_plot()`](https://ropensci.github.io/fellingdater/reference/sw_combine_plot.md)
  :

  Plot the output of
  [`sw_combine()`](https://ropensci.github.io/fellingdater/reference/sw_combine.md)

- [`sw_sum()`](https://ropensci.github.io/fellingdater/reference/sw_sum.md)
  : Compute the summed probability of multiple felling date ranges

- [`sw_sum_plot()`](https://ropensci.github.io/fellingdater/reference/sw_sum_plot.md)
  :

  Plot the output of
  [`sw_sum()`](https://ropensci.github.io/fellingdater/reference/sw_sum.md)

## Synthetic tree-ring series

- [`trs_pseudo_rwl()`](https://ropensci.github.io/fellingdater/reference/trs_pseudo_rwl.md)
  : Create a pseudo-population of tree-ring series

## Tree-ring data manipulation

- [`trs_select()`](https://ropensci.github.io/fellingdater/reference/trs_select.md)
  : Select series from an rwl-style data frame
- [`trs_remove()`](https://ropensci.github.io/fellingdater/reference/trs_remove.md)
  : Removes one or more series from an rwl-style data frame
- [`trs_trim()`](https://ropensci.github.io/fellingdater/reference/trs_trim.md)
  : Trim leading and trailing rows with only NA values from a tree-ring
  data frame
- [`trs_plot_rwl()`](https://ropensci.github.io/fellingdater/reference/trs_plot_rwl.md)
  : Plot all series in a tree-ring width dataframe

## Tree-ring dating

- [`trs_crossdate()`](https://ropensci.github.io/fellingdater/reference/trs_crossdate.md)
  : Sliding window crossdating analysis using multiple statistical
  methods
- [`trs_end_date()`](https://ropensci.github.io/fellingdater/reference/trs_end_date.md)
  : Assign calendar years to tree-ring series in an rwl-style data frame
- [`trs_plot_dated()`](https://ropensci.github.io/fellingdater/reference/trs_plot_dated.md)
  : Plot a dated tree-ring series with a reference series or chronology

## Helper functions

- [`read_fh()`](https://ropensci.github.io/fellingdater/reference/read_fh.md)
  : Read a Heidelberg format (.fh) tree-ring file

- [`fh_header()`](https://ropensci.github.io/fellingdater/reference/fh_header.md)
  : Retrieve the HEADER fields of a Heidelberg format (.fh) file

- [`cor_table()`](https://ropensci.github.io/fellingdater/reference/cor_table.md)
  : Calculate correlation values between tree-ring series

- [`sw_data_overview()`](https://ropensci.github.io/fellingdater/reference/sw_data_overview.md)
  :

  Overview of available sapwood data sets in the `fellingdater` package

- [`sw_data_info()`](https://ropensci.github.io/fellingdater/reference/sw_data_info.md)
  : Detailed information on a sapwood data set

- [`hdi()`](https://ropensci.github.io/fellingdater/reference/hdi.md) :
  Compute the highest posterior density interval (hdi)

- [`mov_av()`](https://ropensci.github.io/fellingdater/reference/mov_av.md)
  : Compute a running mean on a time series

- [`trs_pv()`](https://ropensci.github.io/fellingdater/reference/trs_pv.md)
  : Computes statistics to describe parallel variations between two
  rwl-style data frames

- [`trs_tSt()`](https://ropensci.github.io/fellingdater/reference/trs_tSt.md)
  : Compute Student's t-statistics from correlation coefficients

- [`trs_tbp()`](https://ropensci.github.io/fellingdater/reference/trs_tbp.md)
  : Compute Baillie & Pilcher-style t-values between all series in two
  rwl-style data frames

- [`trs_tbp_transform()`](https://ropensci.github.io/fellingdater/reference/trs_tbp_transform.md)
  : Pre-transform tree-ring series using Baillie & Pilcher method

- [`trs_tho()`](https://ropensci.github.io/fellingdater/reference/trs_tho.md)
  : Compute Hollstein-style t-values between all series in two rwl-style
  data frames

- [`trs_tho_transform()`](https://ropensci.github.io/fellingdater/reference/trs_tho_transform.md)
  : Pre-transform tree-ring series using Hollstein method

- [`trs_zscore()`](https://ropensci.github.io/fellingdater/reference/trs_zscore.md)
  : Standardize tree-ring series to z-scores

- [`sgc_for_plot()`](https://ropensci.github.io/fellingdater/reference/sgc_for_plot.md)
  : Detect synchronous growth changes between two tree-ring series (
  helper for trs_plot_dated() )

## Datasets

- [`Brathen_1982`](https://ropensci.github.io/fellingdater/reference/Brathen_1982.md)
  : Brathen 1982 sapwood data set.
- [`Hollstein_1980`](https://ropensci.github.io/fellingdater/reference/Hollstein_1980.md)
  : Hollstein 1980 sapwood data set.
- [`Miles_1997_NM`](https://ropensci.github.io/fellingdater/reference/Miles_1997_NM.md)
  : Miles 1997 sapwood data set.
- [`Miles_1997_SC`](https://ropensci.github.io/fellingdater/reference/Miles_1997_SC.md)
  : Miles 1997 sapwood data set.
- [`Miles_1997_WBC`](https://ropensci.github.io/fellingdater/reference/Miles_1997_WBC.md)
  : Miles 1997 sapwood data set.
- [`Pilcher_1987`](https://ropensci.github.io/fellingdater/reference/Pilcher_1987.md)
  : Pilcher 1987 sapwood data set.
- [`Sohar_2012_ELL_c`](https://ropensci.github.io/fellingdater/reference/Sohar_2012_ELL_c.md)
  : Sohar et al. 2012 sapwood data set.
- [`Sohar_2012_ELL_t`](https://ropensci.github.io/fellingdater/reference/Sohar_2012_ELL_t.md)
  : Sohar et al. 2012 sapwood data set.
- [`Sohar_2012_FWE_c`](https://ropensci.github.io/fellingdater/reference/Sohar_2012_FWE_c.md)
  : Sohar et al. 2012 sapwood data set.
- [`Sohar_2012_FWE_t`](https://ropensci.github.io/fellingdater/reference/Sohar_2012_FWE_t.md)
  : Sohar et al. 2012 sapwood data set.
- [`vanDaalen_NLBE`](https://ropensci.github.io/fellingdater/reference/vanDaalen_NLBE.md)
  : van Daalen (unpublished) sapwood data set.
- [`vanDaalen_Norway`](https://ropensci.github.io/fellingdater/reference/vanDaalen_Norway.md)
  : van Daalen (unpublished) sapwood data set.
- [`Wazny_1990`](https://ropensci.github.io/fellingdater/reference/Wazny_1990.md)
  : Wazny 1990 sapwood data set.
- [`Weitz_2025`](https://ropensci.github.io/fellingdater/reference/Weitz_2025.md)
  : Weitz 2025 sapwood data set.

## Example data

- [`sw_example0`](https://ropensci.github.io/fellingdater/reference/sw_example0.md)
  : Example dataset 0
- [`sw_example1`](https://ropensci.github.io/fellingdater/reference/sw_example1.md)
  : Example dataset 1
- [`sw_example2`](https://ropensci.github.io/fellingdater/reference/sw_example2.md)
  : Example dataset 2
- [`sw_example3`](https://ropensci.github.io/fellingdater/reference/sw_example3.md)
  : Example dataset 3
- [`sw_example4`](https://ropensci.github.io/fellingdater/reference/sw_example4.md)
  : Example dataset 4
- [`sw_example5`](https://ropensci.github.io/fellingdater/reference/sw_example5.md)
  : Example dataset 5
- [`sw_example6`](https://ropensci.github.io/fellingdater/reference/sw_example6.md)
  : Example dataset 6
- [`sw_example7`](https://ropensci.github.io/fellingdater/reference/sw_example7.md)
  : Example dataset 7

## About

- [`fellingdater-package`](https://ropensci.github.io/fellingdater/reference/fellingdater.md)
  [`fellingdater`](https://ropensci.github.io/fellingdater/reference/fellingdater.md)
  : fellingdater: Tree-ring dating and estimating felling dates of
  historical timbers
