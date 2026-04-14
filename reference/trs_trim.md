# Trim leading and trailing rows with only NA values from a tree-ring data frame

Removes rows at the beginning and end of a `data.frame` of class `"rwl"`
that contain only missing values across all series.

## Usage

``` r
trs_trim(x, rownames_to_years = FALSE)
```

## Arguments

- x:

  A `data.frame` or matrix where each column represents a tree-ring
  series and rows are years. Rownames must be numeric years.

- rownames_to_years:

  Logical. If `TRUE`, the rownames (years) will be transferred to a new
  column named `year`.

## Value

A trimmed `data.frame` with all leading and trailing all-NA rows
removed. If `rownames_to_years = TRUE`, a column `year` is added.

## Examples

``` r
x <- trs_pseudo_rwl(n_series = 3, series_length = 80, end_date = 1990)
x[1:5, ] <- NA # Add leading NA rows
x[81:85, ] <- NA # Add trailing NA rows
dim(x)
#> [1] 85  3
x_trim <- trs_trim(x)
dim(x_trim)
#> [1] 75  3

# With year column added
head(trs_trim(x, rownames_to_years = TRUE))
#>      year trs_1 trs_2 trs_3
#> 1916 1916   108    99    97
#> 1917 1917   112   105   100
#> 1918 1918   116   110   111
#> 1919 1919   120   119   117
#> 1920 1920   111   109   104
#> 1921 1921   109   112   105
```
