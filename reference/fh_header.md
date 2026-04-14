# Retrieve the HEADER fields of a Heidelberg format (.fh) file

This function reports the HEADER fields from a Heidelberg format (.fh)
ring-width file. The header fields are harvested from the .fh-file by
the
[`read_fh()`](https://ropensci.github.io/fellingdater/reference/read_fh.md)
function, which stores the HEADER fields from the .fh file as attributes
of the `data.frame` with the measurement data it returns.

## Usage

``` r
fh_header(x)
```

## Arguments

- x:

  The output of `read_fh(x, header = TRUE)`, a `data.frame` of class
  `rwl`.

## Value

A `data.frame` with 29 header fields.

## Examples

``` r
Doel1 <- system.file("extdata", "DOEL1.fh", package = "fellingdater")
Doel1_trs <- read_fh(Doel1, verbose = FALSE)
fh_header(Doel1_trs)
#>     series data_type         chrono_members species first last length n_sapwood
#> 1   K1_091    Single                   <NA>    QUSP  1158 1292    135        15
#> 2   S38-BB    Single                   <NA>    QUSP  1193 1306    114         0
#> 3  GD3-1BB    Single                   <NA>    QUSP  1222 1310     89         5
#> 4   GR1mBB    Quadro K1_001,K1_004x,GR1-3BB    QUSP  1220 1310     91         3
#> 5   GQ1mBB    Quadro  GQ1-2BB,K1_007,K1_009    QUSP  1150 1314    165         7
#> 6   K1_095    Single                   <NA>    QUSP  1207 1320    114        21
#> 7  GG1-1BB    Single                   <NA>    QUSP  1240 1322     83        13
#> 8   S13mSB    Quadro          S1-3SB,K1_076    QUSP  1164 1322    159        20
#> 9  S13A-BB    Single                   <NA>    QUSP  1232 1324     93        19
#> 10   S6-SB    Single                   <NA>    QUSP  1221 1324    104        14
#>    n_sapwood_chr unmeasured_rings invalid_rings status waneyedge bark pith
#> 1           <NA>               NA            NA  Dated      <NA> <NA>    -
#> 2           <NA>               NA            NA  Dated      <NA> <NA>    -
#> 3           <NA>               NA            NA  Dated      <NA> <NA>    -
#> 4           <NA>               NA            NA  Dated       ---    -    -
#> 5           <NA>               NA            NA  Dated       ---    -    -
#> 6           <NA>               NA            NA  Dated      <NA>    -    -
#> 7           <NA>               NA            NA  Dated      <NA> <NA> <NA>
#> 8           <NA>               NA            NA  Dated       --- <NA>    -
#> 9           <NA>               NA             1  Dated       WKE <NA> <NA>
#> 10          <NA>               NA             1  Dated       WKE <NA> <NA>
#>    pith_offset pith_offset_delta                                  comments
#> 1           NA                NA                                 keelplank
#> 2           NA                NA  HW/SW boundary | K1_281 | framing timber
#> 3           NA                NA                       K1_370 | hull plank
#> 4           NA                NA                                hull plank
#> 5           NA                NA                                hull plank
#> 6           NA                NA                                inner stem
#> 7           NA                NA                                hull plank
#> 8           NA                NA average of S1-3SB,K1_076 | framing timber
#> 9           NA                NA                            framing timber
#> 10          NA                NA                            framing timber
#>                project                location town  zip       street
#> 1  Ship timbers DOEL 1       Doel_Deurganckdok <NA> <NA>         <NA>
#> 2  Ship timbers DOEL 1       Doel_Deurganckdok <NA> <NA>         <NA>
#> 3  Ship timbers DOEL 1       Doel_Deurganckdok <NA> <NA>         <NA>
#> 4  Ship timbers DOEL 1       Doel_Deurganckdok Doel <NA> Deurganckdok
#> 5  Ship timbers DOEL 1       Doel_Deurganckdok Doel <NA> Deurganckdok
#> 6  Ship timbers DOEL 1       Doel_Deurganckdok Doel <NA> Deurganckdok
#> 7                 <NA> KOGGE ANTWERPEN GG1-1BB <NA> <NA>         <NA>
#> 8  Ship timbers DOEL 1       Doel_Deurganckdok <NA> <NA>         <NA>
#> 9  Ship timbers DOEL 1       Doel_Deurganckdok <NA> <NA>         <NA>
#> 10                <NA>   KOGGE ANTWERPEN S6-SB <NA> <NA>         <NA>
#>    sampling_date measuring_date personal_id client_id longitude  latitude
#> 1           <NA>           <NA>          KH      <NA>  4.269711 51.298236
#> 2           <NA>           <NA>          KH      <NA>  4.269711 51.298236
#> 3           <NA>           <NA>          KH      <NA>  4.269711 51.298236
#> 4           <NA>           <NA>          KH      <NA>  4.269711 51.298236
#> 5           <NA>           <NA>          KH      <NA>  4.269711 51.298236
#> 6           <NA>           <NA>          KH      <NA>  4.269711 51.298236
#> 7           <NA>           <NA>        <NA>      <NA>  4.269711 51.298236
#> 8           <NA>           <NA>          KH      <NA>  4.269711 51.298236
#> 9           <NA>           <NA>        <NA>      <NA>  4.269711 51.298236
#> 10          <NA>           <NA>        <NA>      <NA>  4.269711 51.298236
```
