# Compute the highest posterior density interval (hdi)

This function returns a matrix with 'upper' and 'lower' limits of the
hdi (*highest density interval*), and the associated probability 'p'.
The function first sorts the input `data.frame` - with columns
'n_sapwood´ and 'p' (the associated probability) - by column 'p' in
decreasing order and then it calculates the HDI by finding the first
value of the sorted probabilities higher than the specified `cred_mass.`
It then finds the indices of the values that are greater than or equal
to this threshold, and uses these indices to find the 'upper' and
'lower' limits of the hdi. The function also calculates the probability
of the interval. The final result is returned as a `matrix` with
'lower', 'upper', and 'p' values.

This function is applied in functions
[sw_model](https://ropensci.github.io/fellingdater/reference/sw_model.md)
and
[sw_interval](https://ropensci.github.io/fellingdater/reference/sw_interval.md).

## Usage

``` r
hdi(x, a = "n_sapwood", b = "p", cred_mass = 0.954)
```

## Arguments

- x:

  A `data.frame` with columns `n_sapwood` and `p` (the associated
  probability). `x` is computed in functions
  [sw_model](https://ropensci.github.io/fellingdater/reference/sw_model.md),
  [sw_interval](https://ropensci.github.io/fellingdater/reference/sw_interval.md),
  and
  [sw_combine](https://ropensci.github.io/fellingdater/reference/sw_combine.md).

- a:

  The name of the column in x that lists the number of sapwood rings.

- b:

  The name of the column in x that lists the the associated probability
  of having n sapwood rings.

- cred_mass:

  A `scalar [0, 1]` specifying the mass within the credible interval
  (default = .954).

## Value

A `matrix` with ´upper´ and ´lower´ limits of the hdi, and the
associated probability ´p´.

## Examples

``` r
tmp <- data.frame(n_sapwood = seq(1,30, 1),
                  p = dnorm(seq(1,30, 1), 15, 5))
hdi(tmp, cred_mass = 0.954)
#>   lower upper         p
#> 1     5    25 0.9645785
```
