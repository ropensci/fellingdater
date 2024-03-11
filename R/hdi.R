#' Compute the highest posterior density interval (hdi)
#'
#' @description This function returns a matrix with 'upper' and 'lower' limits
#'  of the hdi (*highest density interval*), and the associated probability 'p'.
#'  The function first sorts the input `data.frame` - with columns 'n_sapwood´
#'  and 'p' (the associated probability) - by column 'p' in decreasing order
#'  and then it calculates the HDI by finding the first value of the sorted
#'  probabilities higher than the specified `cred_mass.` It then finds the
#'  indices of the values that are greater than or equal to this threshold, and
#'  uses these indices to find the 'upper' and 'lower' limits of the hdi.
#'  The function also calculates the probability of the interval. The final
#'  result is returned as a `matrix` with 'lower', 'upper', and 'p' values.
#'
#'  This function is applied in functions [sw_model] and [sw_interval].
#'
#' @param x A `data.frame` with columns ´n_sapwood´ and 'p' (the associated
#'  probability). `x` is computed in functions [sw_model], [sw_interval],
#'  and [sw_combine].
#' @param a The name of the column in x that lists the number of sapwood rings.
#' @param b The name of the column in x that lists the the associated
#'   probability of having n sapwood rings.
#' @param cred_mass A `scalar [0, 1]` specifying the mass within the credible
#'  interval (default = .954).
#'
#' @return A `matrix` with ´upper´ and ´lower´ limits of the hdi, and the
#'   associated probability ´p´.
#'
#' @examples
#' tmp <- data.frame(n_sapwood = seq(1,30, 1),
#'                   p = dnorm(seq(1,30, 1), 15, 5))
#' hdi(tmp, cred_mass = 0.954)
#'
#' @export

hdi <- function(x,
                a = "n_sapwood",
                b = "p",
                cred_mass = 0.954) {
     hdi_interval <- c(NA_real_, NA_real_, NA_real_)
     df <- data.frame(n = as.numeric(x[, a]),
                      p = as.numeric(x[, b]))
     df <- df[df$p > 0, ]
     # a vector with sorted probabilities
     df_sorted <- sort(df$p, decreasing = TRUE)
     # index of (first value) df_sorted higher than cred_mass
     outside_idx <- which(cumsum(df_sorted) >= sum(df$p) * cred_mass)
     outside_idx <- min(outside_idx)
     upper = df_sorted[outside_idx]
     indices = which(df$p >= upper)
     gaps <- which(diff(indices) > 1)
     starts <- indices[c(1, gaps + 1)]
     ends <- indices[c(gaps, length(indices))]
     p_interval <- as.integer(length(starts))
     for (j in 1:length(starts))
     {
          p_interval[j] <- sum(df$p[starts[j]:ends[j]])
     }
     hdi_interval <- cbind(lower = df$n[starts],
                           upper = df$n[ends],
                           p = p_interval)
     hdi_interval <- as.data.frame(hdi_interval)

     return(hdi_interval)
}
