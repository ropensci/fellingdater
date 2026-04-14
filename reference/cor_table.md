# Calculate correlation values between tree-ring series

**Deprecated**: `cor_table()` is deprecated as of fellingdater version
1.2.0 Please use
[`trs_crossdate()`](https://ropensci.github.io/fellingdater/reference/trs_crossdate.md)
instead.

This function calculates common correlation values between dated
tree-ring series (`x`) and a set of reference chronologies (`y`). When
no master chronologies are provided, each series in x is compared to all
other series in x.

Only values are reported for pairs of series with a common overlap \>=
min_overlap.

The correlation values computed include:

- glk: 'Gleichläufigkeit' or 'percentage of parallel variation' (Buras &
  Wilmking 2015; Eckstein & Bauch 1969; Huber 1942; Visser 2020)

- glk_p: significance level associated with the glk-value (Jansma 1995)

- r_pearson: the Pearson's correlation coefficient

- t_St: Student's t-value

- t_BP: t-values according to the Baillie & Pilcher (1973) algorithm

- t_Ho: t-values according to the Hollstein (1980) algorithm

## Usage

``` r
cor_table(
  x,
  y = NULL,
  min_overlap = 50,
  remove_duplicates = TRUE,
  output = "table",
  sort_by = "t_Ho"
)
```

## Arguments

- x:

  A `data.frame` of class ´rwl' with tree-ring data. Each column
  represents a measurement series, and row names correspond to
  (calendar) years.

- y:

  A `data.frame` of class 'rwl' with tree-ring data. Each column
  represents a measurement series or chronology, and row names
  correspond to (calendar) years. If NULL (default), `x` is compared to
  itself (y = x).

- min_overlap:

  A `numeric` value specifying the minimum overlap required between
  series for correlation calculation.

- remove_duplicates:

  A logical value. If `TRUE`, identical pairs of series and references
  are removed from the output.

- output:

  The desired output format, either "matrix" or "table" (default).

- sort_by:

  The correlation value by which the output is sorted for each series in
  `x`. One of "r_pearson", "t_St", "glk", "glk_p", "t_BP", "t_Ho".
  Default to "t_Ho"

## Value

The function returns a list of correlation matrices if `output` is set
to "matrix." If `output` is set to "table," it returns a `data.frame`
reporting all correlation values.

## Details

This function computes various correlation values between tree-ring
series in a `data.frame` `x` and a set of reference chronologies in a
`data.frame` `y`. If `y` is not provided, it compares each series in `x`
to all other series in \`x.

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

[`trs_crossdate()`](https://ropensci.github.io/fellingdater/reference/trs_crossdate.md)
for the function that replaces `cor_table())`.

## Examples

``` r
# example code
if (FALSE) { # \dontrun{
Doel1 <- system.file("extdata", "DOEL1.fh", package = "fellingdater")
Doel1_trs <- read_fh(Doel1, verbose = FALSE)
# crossdating ring-width series from Doel 1 against each other:

cor_results <- cor_table(Doel1_trs,
     output = "table", min_overlap = 80,
     sort_by = "t_Ho", remove_duplicates = TRUE
)
head(cor_results, 20)
} # }
```
