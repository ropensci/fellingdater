# Computes the limits of the felling date range

This function computes the probability density function (PDF) and the
highest posterior density interval (HDI) of the felling date range,
based on the observed number of sapwood rings, their dating, and a
selected sapwood dataset and model.

## Usage

``` r
sw_interval(
  n_sapwood = NA,
  last = 1,
  hdi = FALSE,
  cred_mass = 0.954,
  sw_data = "Hollstein_1980",
  densfun = "lognormal",
  plot = FALSE
)
```

## Arguments

- n_sapwood:

  A `numeric`. The number of observed sapwood rings.

- last:

  A `numeric`. The calendar year assigned to the outermost sapwood ring
  (default = 1).

- hdi:

  Logical. If `TRUE`, the function returns the lower and upper bounds of
  the highest density interval (HDI). If `FALSE`, it returns a data
  frame with scaled probability values for each possible number of
  sapwood rings.

- cred_mass:

  A numeric `scalar` in the range of `[0, 1]`, specifying the mass of
  the credible interval (default = .954)

- sw_data:

  The name of the sapwood data set to use for modelling. It should be
  one of the data sets listed in
  [`sw_data_overview()`](https://ropensci.github.io/fellingdater/reference/sw_data_overview.md),
  or the name of a `data.frame` with sapwood data in columns `n_sapwood`
  and `count`.

- densfun:

  A character string naming the distribution to fit. One of:

  - *lognormal* (default),

  - *normal*,

  - *weibull*,

  - *gamma*.

- plot:

  A `logical`.

  - If `TRUE`, a ggplot-style graph is returned of the individual
    sapwood model and estimate of the felling date range.

  - If `FALSE`, a `list` with the numeric output of the modelling
    process is returned.

## Value

The type of output depends on the value of `hdi`:

- If `hdi = TRUE`, a numeric vector with the lower and upper limits of
  the highest density interval (HDI). Attributes include `cred_mass`,
  `sw_data`, and the fitted `model`.

- If `hdi = FALSE`, a data frame with scaled probability values (`p`)
  for each year after the last dated sapwood ring.

## Examples

``` r
# 10 sapwood rings observed and the Wazny 1990 sapwood model:
sw_interval(
     n_sapwood = 10,
     last = 1234,
     hdi = TRUE,
     cred_mass = .95,
     sw_data = "Wazny_1990",
     densfun = "lognormal",
     plot = FALSE
)
#>   lower upper         p
#> 1  1234  1250 0.9611793
# same example as above, but with numerical output (hdi = FALSE):
sw_interval(
     n_sapwood = 10,
     last = 1234,
     hdi = FALSE,
     cred_mass = .95,
     sw_data = "Wazny_1990",
     densfun = "lognormal",
     plot = FALSE
)
#>     year n_sapwood            p
#> 1   1234        10 4.387611e-02
#> 2   1235        11 6.219097e-02
#> 3   1236        12 7.801314e-02
#> 4   1237        13 8.894482e-02
#> 5   1238        14 9.400306e-02
#> 6   1239        15 9.347466e-02
#> 7   1240        16 8.845988e-02
#> 8   1241        17 8.038628e-02
#> 9   1242        18 7.064400e-02
#> 10  1243        19 6.037958e-02
#> 11  1244        20 5.042220e-02
#> 12  1245        21 4.129538e-02
#> 13  1246        22 3.327159e-02
#> 14  1247        23 2.643959e-02
#> 15  1248        24 2.076722e-02
#> 16  1249        25 1.615214e-02
#> 17  1250        26 1.245872e-02
#> 18  1251        27 9.542758e-03
#> 19  1252        28 7.266294e-03
#> 20  1253        29 5.505594e-03
#> 21  1254        30 4.154352e-03
#> 22  1255        31 3.124045e-03
#> 23  1256        32 2.342677e-03
#> 24  1257        33 1.752749e-03
#> 25  1258        34 1.309002e-03
#> 26  1259        35 9.762245e-04
#> 27  1260        36 7.272799e-04
#> 28  1261        37 5.414149e-04
#> 29  1262        38 4.028596e-04
#> 30  1263        39 2.996922e-04
#> 31  1264        40 2.229393e-04
#> 32  1265        41 1.658698e-04
#> 33  1266        42 1.234492e-04
#> 34  1267        43 9.192023e-05
#> 35  1268        44 6.848429e-05
#> 36  1269        45 5.105942e-05
#> 37  1270        46 3.809853e-05
#> 38  1271        47 2.845283e-05
#> 39  1272        48 2.126964e-05
#> 40  1273        49 1.591626e-05
#> 41  1274        50 1.192322e-05
#> 42  1275        51 8.942096e-06
#> 43  1276        52 6.714252e-06
#> 44  1277        53 5.047602e-06
#> 45  1278        54 3.799408e-06
#> 46  1279        55 2.863535e-06
#> 47  1280        56 2.161002e-06
#> 48  1281        57 1.632988e-06
#> 49  1282        58 1.235643e-06
#> 50  1283        59 9.362498e-07
#> 51  1284        60 7.103687e-07
#> 52  1285        61 5.397257e-07
#> 53  1286        62 4.106413e-07
#> 54  1287        63 3.128631e-07
#> 55  1288        64 2.386985e-07
#> 56  1289        65 1.823682e-07
#> 57  1290        66 1.395252e-07
#> 58  1291        67 1.068956e-07
#> 59  1292        68 8.201055e-08
#> 60  1293        69 6.300581e-08
#> 61  1294        70 4.847197e-08
#> 62  1295        71 3.734200e-08
#> 63  1296        72 2.880707e-08
#> 64  1297        73 2.225318e-08
#> 65  1298        74 1.721367e-08
#> 66  1299        75 1.333338e-08
#> 67  1300        76 1.034162e-08
#> 68  1301        77 8.031835e-09
#> 69  1302        78 6.246191e-09
#> 70  1303        79 4.863914e-09
#> 71  1304        80 3.792475e-09
#> 72  1305        81 2.960884e-09
#> 73  1306        82 2.314610e-09
#> 74  1307        83 1.811705e-09
#> 75  1308        84 1.419861e-09
#> 76  1309        85 1.114164e-09
#> 77  1310        86 8.753705e-10
#> 78  1311        87 6.886052e-10
#> 79  1312        88 5.423498e-10
#> 80  1313        89 4.276761e-10
#> 81  1314        90 3.376542e-10
#> 82  1315        91 2.668986e-10
#> 83  1316        92 2.112190e-10
#> 84  1317        93 1.673507e-10
#> 85  1318        94 1.327473e-10
#> 86  1319        95 1.054198e-10
#> 87  1320        96 8.381333e-11
#> 88  1321        97 6.671042e-11
#> 89  1322        98 5.315687e-11
#> 90  1323        99 4.240389e-11
#> 91  1324       100 3.386323e-11
#> 92  1325       101 2.707215e-11
#> 93  1326       102 2.166629e-11
#> 94  1327       103 1.735839e-11
#> 95  1328       104 1.392172e-11
#> 96  1329       105 1.117714e-11
#> 97  1330       106 8.982951e-12
#> 98  1331       107 7.226918e-12
#> 99  1332       108 5.820081e-12
#> 100 1333       109 4.691834e-12
#> 101 1334       110 3.786080e-12
```
