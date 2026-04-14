# Read a Heidelberg format (.fh) tree-ring file

This function reads a Heidelberg format file (.fh) containing ring width
data and returns the HEADER fields as attributes of the
`data.frame`´with the measurement data.

The Heidelberg file format is described in detail here:
https://www.treeringsociety.org/resources/SOM/Brewer_Murphy_SupplementaryMaterial.pdf

## Usage

``` r
read_fh(fname, BC_correction = FALSE, verbose = TRUE, header = FALSE)
```

## Arguments

- fname:

  A `character` vector specifying the name of the .fh file to read.

- BC_correction:

  A `logical`. If `TRUE` the correction moves BC-dates one year forward.

- verbose:

  A `logical`. If `FALSE`, messages are suppressed during the reading
  process.

- header:

  A `logical`. If `TRUE` the HEADER fields are returned as a
  `data.frame`; if `FALSE`, the measurement data is returned.

## Value

If `header` is TRUE, a data.frame is returned with HEADER fields as
attributes. If `header` is FALSE, a `data.frame` of class `rwl` with
ring-width measurements in columns is returned, with (calendar) years as
row names.\`

## Details

This function reads .fh file with ring width data in either block
(decadal format) or column format (e.g., with comment flags) as used by
TSAP program. The function is also capable of reading chronologies or
half-chronos in decadal format. The `read_fh` function is
case-insensitive. Information found in the HEADER fields is listed as
attributes of the `data.frame`´with the measurement data. .

The header fields harvested from the .fh file include:

"Project", "FirstMeasurementDate", "Location", "Town", "Street",
"Client", "Longitude", "Latitude", "DateOfSampling",
"FirstMeasurementDate", "SapWoodRings", "Comment", "MissingRingsAfter",
"InvalidRingsAfter", "MissingringsBefore", "DeltaMissingringsBefore",
"ChronoMemberKeycodes", "PersId"

## References

This function is an extension of `read.fh()` from the **dplR package**
(<https://github.com/opendendro/dplR>), developed and maintained by
Prof. dr. Andy Bunn (Bunn 2008, Bunn 2010, Bunn et al. 2022) on
<https://opendendro.org/>.

## Author

The original `read.fh()` function is part of the **dplR package**
(<https://github.com/opendendro/dplR>) and was developed by Christian
Zang, with new features and patches contributed by Mikko Korpela and
Ronald Visser. This `read_fh()` function expands the functionalities of
the original
[`dplR::read.fh()`](https://rdrr.io/pkg/dplR/man/read.fh.html).

## Examples

``` r
Doel1 <- system.file("extdata", "DOEL1.fh", package = "fellingdater")
Doel1_trs <- read_fh(Doel1, verbose = FALSE)
head(Doel1_trs, 10)
#>      K1_091 S38-BB GD3-1BB GR1mBB GQ1mBB K1_095 GG1-1BB S13mSB S13A-BB S6-SB
#> 1150     NA     NA      NA     NA   3.80     NA      NA     NA      NA    NA
#> 1151     NA     NA      NA     NA   4.15     NA      NA     NA      NA    NA
#> 1152     NA     NA      NA     NA   4.63     NA      NA     NA      NA    NA
#> 1153     NA     NA      NA     NA   4.31     NA      NA     NA      NA    NA
#> 1154     NA     NA      NA     NA   3.97     NA      NA     NA      NA    NA
#> 1155     NA     NA      NA     NA   4.10     NA      NA     NA      NA    NA
#> 1156     NA     NA      NA     NA   2.02     NA      NA     NA      NA    NA
#> 1157     NA     NA      NA     NA   3.24     NA      NA     NA      NA    NA
#> 1158   2.50     NA      NA     NA   3.75     NA      NA     NA      NA    NA
#> 1159   2.73     NA      NA     NA   3.07     NA      NA     NA      NA    NA

Doel1_header <- read_fh(Doel1, verbose = FALSE, header = TRUE)
Doel1_header
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
