#' Computes the limits of the felling date range
#'
#' @description This function computes the probability density function (PDF)
#'   and the highest posterior density interval (HDI) of the felling date range,
#'   based on the observed number of sapwood rings, their dating, and a selected
#'   sapwood dataset and model.
#'
#' @param n_sapwood A `numeric`. The number of observed sapwood rings.
#' @param last A `numeric`. The calendar year assigned to the outermost sapwood
#'   ring (default = 1).
#' @param hdi Logical. If `TRUE`, the function returns the lower and upper bounds
#' of the highest density interval (HDI). If `FALSE`, it returns a data frame with
#' scaled probability values for each possible number of sapwood rings.
#' @param cred_mass  A numeric `scalar` in the range of `[0, 1]`, specifying the
#'   mass of the credible interval (default = .954)
#' @param sw_data The name of the sapwood data set to use for modelling. It
#'   should be one of the data sets listed in [sw_data_overview()], or the name
#'   of a `data.frame` with sapwood data in columns `n_sapwood` and `count`.
#' @param densfun A character string naming the distribution to fit.
#'   One of:
#'   * _lognormal_ (default),
#'   * _normal_,
#'   * _weibull_,
#'   * _gamma_.
#' @param plot A `logical`.
#'   * If `TRUE`, a ggplot-style graph is returned of the individual
#'   sapwood model and estimate of the felling date range.
#'   * If `FALSE`, a `list` with the numeric output of the modelling process is returned.
#' @examples
#' # 10 sapwood rings observed and the Wazny 1990 sapwood model:
#' sw_interval(
#'      n_sapwood = 10,
#'      last = 1234,
#'      hdi = TRUE,
#'      cred_mass = .95,
#'      sw_data = "Wazny_1990",
#'      densfun = "lognormal",
#'      plot = FALSE
#' )
#' # same example as above, but with numerical output (hdi = FALSE):
#' sw_interval(
#'      n_sapwood = 10,
#'      last = 1234,
#'      hdi = FALSE,
#'      cred_mass = .95,
#'      sw_data = "Wazny_1990",
#'      densfun = "lognormal",
#'      plot = FALSE
#' )
#'
#' @export
#'
#' @return The type of output depends on the value of `hdi`:
#'
#' * If `hdi = TRUE`, a numeric vector with the lower and upper limits of the
#'   highest density interval (HDI). Attributes include `cred_mass`, `sw_data`,
#'   and the fitted `model`.
#' * If `hdi = FALSE`, a data frame with scaled probability values (`p`) for
#'   each year after the last dated sapwood ring.

sw_interval <-
     function(n_sapwood = NA,
              last = 1,
              hdi = FALSE,
              cred_mass = 0.954,
              sw_data = "Hollstein_1980",
              densfun = "lognormal",
              plot = FALSE) {
          check_densfun(densfun)
          check_plot(plot)
          check_cred_mass(cred_mass)
          check_n_sapwood(n_sapwood)

          if (!is.numeric(last)) {
               stop("--> `last` must be a numeric value")
          }

          sw_model_params <- sw_model(
               sw_data = sw_data,
               densfun = densfun,
               cred_mass = cred_mass,
               plot = FALSE
          )
          a <- sw_model_params$fit_parameters$estimate[1]
          sigma <- sw_model_params$fit_parameters$estimate[2]
          if (n_sapwood > sw_model_params$range[3]) {
               warning(
                    paste0(
                         "--> ",
                         n_sapwood,
                         " lies outside the range of the observed number of sapwood rings for the ",
                         sw_data, " data set. Is this a correct value?"
                    )
               )
          }

          swr_n <- seq(n_sapwood, n_sapwood + 100, by = 1)
          year <- seq(last, last + 100, by = 1)
          p <- d_dens(
               densfun = densfun,
               x = swr_n,
               param1 = a,
               param2 = sigma,
               n = 1
          )
          pdf <- data.frame(year, swr_n, p)
          colnames(pdf) <- c("year", "n_sapwood", "p")

          # Filter extreme low p values (e.g. when a very high no. of swr is observed).
          # If not filtered, a hdi is computed based on extremely low p-values.
          # 0.001 is an arbitrarily chosen threshold value.
          if (sum(pdf$p) < 0.001) {
               stop(
                    paste0(
                         "--> Sapwood numbers of >= ",
                         n_sapwood,
                         " are very unlikely (p < .001).
This value falls outside the range of the chosen sapwood model."
                    )
               )
          }

          # scale density function to 1
          pdf$p <- pdf$p / sum(pdf$p)

          # compute limits of hdi-interval
          hdi_int <- hdi(
               x = pdf[, -1],
               a = "n_sapwood",
               b = "p",
               cred_mass = cred_mass
          )

          hdi_int[1] <- hdi_int[[1]] - n_sapwood + last
          hdi_int[2] <- hdi_int[[2]] - n_sapwood + last

          attr(pdf, "sapwood_data") <- sw_data
          attr(pdf, "model") <- densfun
          attr(pdf, "cred_mass") <- cred_mass
          attr(pdf, "hdi") <- hdi_int

          if (nrow(pdf) <= 1 && hdi) {
               hdi_int <- c(lower = last, upper = NA_integer_, p = NA_real_)
               attr(hdi_int, "cred_mass") <- cred_mass
               attr(hdi_int, "sapwood_data") <- sw_data
               attr(hdi_int, "model") <- densfun
               warning("--> No upper limit for the hdi could be computed.")
               return(hdi_int)
          }

          if (!hdi && !plot) {
               return(pdf)
          }

          if (plot) {
               return(sw_interval_plot(x = pdf))
          }
          attr(hdi_int, "cred_mass") <- cred_mass
          attr(hdi_int, "sapwood_data") <- sw_data
          attr(hdi_int, "model") <- densfun

          return(hdi_int)
     }
