# Assign calendar years to tree-ring series in an rwl-style data frame

Sets the row names (years) of an rwl-style data frame based on a
specified end year. Optionally trims trailing rows with only `NA`
values.

## Usage

``` r
trs_end_date(x, end_year = 2025, trim = FALSE)
```

## Arguments

- x:

  A `data.frame` of class `"rwl"`, with each column representing a
  tree-ring series.

- end_year:

  An integer. The calendar year assigned to the most recent tree ring
  (last row).

- trim:

  Logical. If `TRUE`, the data frame is trimmed to remove leading and
  trailing rows with no data (see
  [`trs_trim`](https://ropensci.github.io/fellingdater/reference/trs_trim.md)).

## Value

A `data.frame` with row names set to calendar years.

## Examples

``` r
rwl <- trs_pseudo_rwl(n_series = 2, series_length = 50)
trs_end_date(rwl, end_year = 2000)
#> End date set for all series in x
#>      trs_1 trs_2
#> 1951   116   112
#> 1952   123   111
#> 1953   107   107
#> 1954   102    89
#> 1955    99    91
#> 1956   114   110
#> 1957   105   104
#> 1958    96    94
#> 1959    82    80
#> 1960    96    97
#> 1961    96   105
#> 1962   108   118
#> 1963   111   118
#> 1964   122   130
#> 1965   124   137
#> 1966   123   128
#> 1967   121   129
#> 1968   118   123
#> 1969   107   112
#> 1970    95   107
#> 1971   109   121
#> 1972   106   108
#> 1973   103   113
#> 1974   102   116
#> 1975   117   121
#> 1976   113   124
#> 1977   105   109
#> 1978   114   111
#> 1979   110   108
#> 1980    96   107
#> 1981    80    87
#> 1982    87   100
#> 1983    83    92
#> 1984    91    84
#> 1985    88    70
#> 1986   113   104
#> 1987   104    95
#> 1988    99    92
#> 1989   109    99
#> 1990    94    94
#> 1991   102    97
#> 1992   103    98
#> 1993   107   105
#> 1994   113   113
#> 1995    95   105
#> 1996   104   116
#> 1997   106   107
#> 1998   102   112
#> 1999   101   106
#> 2000   101   102
trs_end_date(rwl, end_year = 2000, trim = TRUE)
#> End date set for all series in x
#>      trs_1 trs_2
#> 1951   116   112
#> 1952   123   111
#> 1953   107   107
#> 1954   102    89
#> 1955    99    91
#> 1956   114   110
#> 1957   105   104
#> 1958    96    94
#> 1959    82    80
#> 1960    96    97
#> 1961    96   105
#> 1962   108   118
#> 1963   111   118
#> 1964   122   130
#> 1965   124   137
#> 1966   123   128
#> 1967   121   129
#> 1968   118   123
#> 1969   107   112
#> 1970    95   107
#> 1971   109   121
#> 1972   106   108
#> 1973   103   113
#> 1974   102   116
#> 1975   117   121
#> 1976   113   124
#> 1977   105   109
#> 1978   114   111
#> 1979   110   108
#> 1980    96   107
#> 1981    80    87
#> 1982    87   100
#> 1983    83    92
#> 1984    91    84
#> 1985    88    70
#> 1986   113   104
#> 1987   104    95
#> 1988    99    92
#> 1989   109    99
#> 1990    94    94
#> 1991   102    97
#> 1992   103    98
#> 1993   107   105
#> 1994   113   113
#> 1995    95   105
#> 1996   104   116
#> 1997   106   107
#> 1998   102   112
#> 1999   101   106
#> 2000   101   102
```
