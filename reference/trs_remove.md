# Removes one or more series from an rwl-style data frame

Removes one or more tree-ring series (columns) from a data frame of
class `"rwl"`. Optionally adds the rownames (assumed to be years) as a
column.

## Usage

``` r
trs_remove(x, series, trim = FALSE)
```

## Arguments

- x:

  A `data.frame` of class `"rwl"`, where each column is a tree-ring
  width series and rownames represent years.

- series:

  A character vector specifying the names of the series (columns) to
  remove.

- trim:

  Logical. If `TRUE`, leading and trailing all-NA rows are removed using
  [`trs_trim()`](https://ropensci.github.io/fellingdater/reference/trs_trim.md).

## Value

A `data.frame` with the specified series removed.

## Examples

``` r
rwl <- trs_pseudo_rwl(n_series = 3, series_length = 60, end_date = 2000)
trs_remove(rwl, series = "trs_1", trim = TRUE)
#>      trs_2 trs_3
#> 1941   115   111
#> 1942   112   109
#> 1943   116   106
#> 1944   117   109
#> 1945   114   109
#> 1946   104    96
#> 1947   109    97
#> 1948   104    96
#> 1949   125   111
#> 1950   114   104
#> 1951   117   110
#> 1952   109   102
#> 1953   101    96
#> 1954    94    95
#> 1955    93    88
#> 1956   101    88
#> 1957   111    98
#> 1958    99    96
#> 1959    93    85
#> 1960   100    97
#> 1961   105   108
#> 1962   101   111
#> 1963    94    97
#> 1964   105   115
#> 1965   108   111
#> 1966   109   116
#> 1967   104   105
#> 1968    89    88
#> 1969    82    78
#> 1970    87    80
#> 1971    83    80
#> 1972    98    81
#> 1973    94    91
#> 1974   104    96
#> 1975   101   109
#> 1976   100   105
#> 1977   101   104
#> 1978   104   111
#> 1979   109   108
#> 1980   105   109
#> 1981    98   101
#> 1982   112   102
#> 1983   116   112
#> 1984   116   105
#> 1985    95    88
#> 1986   106    97
#> 1987    98    98
#> 1988    96    98
#> 1989   117   114
#> 1990   115   115
#> 1991   107   104
#> 1992   103    94
#> 1993   105   104
#> 1994   103   108
#> 1995   104   104
#> 1996   110   105
#> 1997   115   118
#> 1998    98   108
#> 1999   103   113
#> 2000   107   111
trs_remove(rwl, series = c("trs_1", "trs_3"))
#>      trs_2
#> 1941   115
#> 1942   112
#> 1943   116
#> 1944   117
#> 1945   114
#> 1946   104
#> 1947   109
#> 1948   104
#> 1949   125
#> 1950   114
#> 1951   117
#> 1952   109
#> 1953   101
#> 1954    94
#> 1955    93
#> 1956   101
#> 1957   111
#> 1958    99
#> 1959    93
#> 1960   100
#> 1961   105
#> 1962   101
#> 1963    94
#> 1964   105
#> 1965   108
#> 1966   109
#> 1967   104
#> 1968    89
#> 1969    82
#> 1970    87
#> 1971    83
#> 1972    98
#> 1973    94
#> 1974   104
#> 1975   101
#> 1976   100
#> 1977   101
#> 1978   104
#> 1979   109
#> 1980   105
#> 1981    98
#> 1982   112
#> 1983   116
#> 1984   116
#> 1985    95
#> 1986   106
#> 1987    98
#> 1988    96
#> 1989   117
#> 1990   115
#> 1991   107
#> 1992   103
#> 1993   105
#> 1994   103
#> 1995   104
#> 1996   110
#> 1997   115
#> 1998    98
#> 1999   103
#> 2000   107
```
