# Sliding window crossdating analysis using multiple statistical methods

This function performs crossdating analysis using a sliding window with
multiple statistical methods: Parallel Variation (GLK/SGC), Baillie &
Pilcher t-values, Hollstein t-values, and standard Pearson correlation
t-values. It tests series at different temporal lags to find a potential
date positions.

## Usage

``` r
trs_crossdate(
  x,
  y = NULL,
  min_overlap = 30,
  sliding = TRUE,
  top_n = NULL,
  rank_by = NULL,
  pb = TRUE
)
```

## Arguments

- x:

  A data frame of test tree-ring series in `rwl` format (years as
  rownames, series as columns). All columns must be numeric.

- y:

  A data frame of reference tree-ring series in `rwl` format. If `NULL`,
  uses `x` for self-comparison. Default is `NULL`.

- min_overlap:

  Integer. Minimum number of overlapping years required between series
  pairs to compute statistics. Must be \>= 3. Default is 30.

- sliding:

  Logical. If `TRUE`, performs sliding window analysis testing multiple
  temporal lags. If `FALSE`, tests only at lag 0 (contemporary
  positioning). Default is `TRUE`.

- top_n:

  Integer or NULL. If specified, returns only the top `n` matches per
  test series, ranked by the statistic given in `rank_by`. Requires
  `rank_by` to be specified. Default is `NULL`.

- rank_by:

  Character or NULL. Statistic to rank by when sorting results per test
  series. If `NULL`, no additional sorting is applied beyond default
  ordering. Options are "r_pearson", "t_St", "t_BP", "t_Ho", "sgc",
  "ssgc", "glk". Default is `NULL`.

- pb:

  Logical. If `TRUE` (default), displays a progress bar during
  computation. Set to `FALSE` to suppress progress bar output, useful
  when calling the function programmatically or in non-interactive
  sessions.

## Value

A data frame with crossdating results containing the following columns:

- series:

  Name of the test series from x

- length:

  Length of the test series

- first:

  Date of the first ring at the particular lag position

- last:

  Date of the last ring at the particular lag position

- reference:

  Name of the reference series

- ref_first:

  Date of the first ring in the reference series

- ref_last:

  Date of the last ring in the reference series

- overlap:

  Number of overlapping years between series and reference

- r_pearson:

  Pearson correlation coefficient

- t_St:

  Student's t-statistic

- t_BP:

  Baillie & Pilcher t-statistic

- t_Ho:

  Hollstein t-statistic

- sgc:

  Synchronous Growth Change (SGC) statistic

- ssgc:

  Semi-SGC statistic

- sgc_p:

  SGC p-value

- glk:

  Gleichläufigkeit (GLK) statistic

- glk_p:

  GLK p-value

## Details

This function implements sliding window crossdating, a fundamental
technique in dendrochronology for dating wood samples of unknown age.
The process involves:

1.  Determining the range of possible lags based on series overlap

2.  For each lag, shifting the test series temporally

3.  Computing all crossdating statistics

4.  Returning results for each tested position

The sliding window approach allows identification of the temporal
position where test and reference series show maximum similarity, which
indicates the most likely dating for the test series.

**Statistical Methods:**

- t_BP: t-values according to the Baillie & Pilcher (1973) algorithm

- t_Ho: t-values according to the Hollstein (1980) algorithm

- sgc, sgc_p: synchronous growth change and associated significance
  level (Visser 2020)

- ssgc: semi-synchronous growth change (Visser 2020)

- glk, glk_p: 'Gleichläufigkeit' or 'percentage of parallel variation'
  (Buras & Wilmking 2015; Eckstein & Bauch 1969; Huber 1942), and
  significance level (p-value) associated with the glk-value (Jansma
  1995)

- r_pearson: the Pearson's correlation coefficient

- t_St: Student's t-value

**Sorting and Filtering:**

- If `rank_by = NULL`: Results are returned with default ordering
  (series, reference, first)

- If `rank_by` is specified: Results are sorted per test series by the
  chosen statistic (high to low)

- If `top_n` is specified: Only the top n matches per test series are
  returned (after optional sorting)

## References

- Baillie, M.G.L., Pilcher, J.R. (1973) A simple crossdating program for
  tree-ring research. *Tree-Ring Bulletin* **33**, 7–14.
  <http://hdl.handle.net/10150/260029>

- Buras, A. and Wilmking, M. (2015) Correcting the calculation of
  Gleichläufigkeit, *Dendrochronologia* **34**, 29-30.
  <https://doi.org/10.1016/j.dendro.2015.03.003>

- Eckstein, D. and Bauch, J. (1969) Beitrag zur Rationalisierung eines
  dendrochronologischen Verfahrens und zur Analyse seiner
  Aussagesicherheit. *Forstwissenschaftliches Centralblatt*, **88**(1),
  230-250.

- Huber, B. (1943) Über die Sicherheit jahrringchronologischer
  Datierung. *Holz als Roh- und Werkstoff* **6**, 263-268.
  <https://doi.org/10.1007/BF02603303>

- Hollstein E. (1980) Mitteleuropäische Eichenchronologie. Trierer
  dendrochronologische Forschungen zur Archäologie und Kunstgeschichte,
  Trierer Grabungen und Forschungen **11**, Mainz am Rhein.

- Jansma, E. (1995) RemembeRINGs; The development and application of
  local and regional tree-ring chronologies of oak for the purposes of
  archaeological and historical research in the Netherlands, Nederlandse
  Archeologische Rapporten **19**, Rijksdienst voor het Oudheidkundig
  Bodemonderzoek, Amersfoort.
  <https://dspace.library.uu.nl/handle/1874/45149>

- Schweingruber, F. H. (1988) Tree rings: basics and applications of
  dendrochronology, Kluwer Academic Publishers, Dordrecht, Netherlands,
  276 p.

- Visser, R.M. (2020) On the similarity of tree-ring patterns: Assessing
  the influence of semi-synchronous growth changes on the
  Gleichläufigkeit for big tree-ring data sets, *Archaeometry* **63**,
  204-215. <https://doi.org/10.1111/arcm.12600>

## See also

[`trs_pv`](https://ropensci.github.io/fellingdater/reference/trs_pv.md),
[`trs_tbp`](https://ropensci.github.io/fellingdater/reference/trs_tbp.md),
[`trs_tho`](https://ropensci.github.io/fellingdater/reference/trs_tho.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create sample data
rwl_test <- trs_pseudo_rwl(n_series = 2, series_length = 80, end_date = 1950)
rwl_ref <- trs_pseudo_rwl(n_series = 3, series_length = 150, end_date = 2000)

# Full sliding window analysis with default ordering
result <- trs_crossdate(rwl_test, rwl_ref, min_overlap = 30)

# Sort by Hollstein t-values
result_sorted <- trs_crossdate(rwl_test, rwl_ref, min_overlap = 30, rank_by = "t_Ho")

# Contemporary positioning only
contemp <- trs_crossdate(rwl_test, rwl_ref, sliding = FALSE)

# Self-comparison for quality control
self_check <- trs_crossdate(rwl_ref, sliding = FALSE)

# Get top 3 matches per series ranked by Hollstein t-values
top_matches <- trs_crossdate(rwl_test, rwl_ref, top_n = 3, rank_by = "t_Ho")
} # }
```
