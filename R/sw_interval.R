#' Computes the limits of the felling date range
#'
#' @description This function computes the probability density function (PDF)
#'   and the highest posterior density interval (hdi) of the felling date range
#'   based on the observed number of sapwood rings, their chronological dating,
#'   and the selected sapwood data and model.
#'
#' @param n_sapwood A `numeric`. The number of observed sapwood rings.
#' @param last A `numeric`. The calendar year assigned to the outermost sapwood
#'   ring (optional, default = 0).
#' @param hdi A `logical.` If `TRUE`, the lower and upper limit of the highest
#'   density interval (credible interval) are returned. When `FALSE`, a matrix
#'   is returned with scaled p-values for each number of observed sapwood rings.
#' @param cred_mass  A `scalar` in the range of `[0, 1]` specifying the mass
#'   within the credible interval (default = .954)
#' @param sw_data The name of the sapwood data set to use for modelling. It
#'   should be one of the data sets listed in [sw_data_overview()], or the name
#'   of a `data.frame` with sapwood data in columns `n_sapwood` and `count`.
#' @param densfun Name of the density function fitted to the sapwood data set.
#'   Should be one of:
#'   * _lognormal_ (the default value),
#'   * _normal_,
#'   * _weibull_,
#'   * _gammma_.
#' @param plot A `logical`.
#'   * If `TRUE`, a ggplot-style graph is returned of the individual
#'   sapwood model and estimate of the felling date range.
#'   * If `FALSE`, a `list` with the numeric output of the modelling process is returned.
#' @examples
#' # 10 sapwood rings observed and the Wazny 1990 sapwood model:
#' sw_interval(
#'   n_sapwood = 10,
#'   last = 1234,
#'   hdi = TRUE,
#'   cred_mass = .95,
#'   sw_data = "Wazny_1990",
#'   densfun = "lognormal",
#'   plot = FALSE
#' )
#' # same example as above, but with numerical output (hdi = FALSE):
#' sw_interval(
#'   n_sapwood = 10,
#'   last = 1234,
#'   hdi = FALSE,
#'   cred_mass = .95,
#'   sw_data = "Wazny_1990",
#'   densfun = "lognormal",
#'   plot = FALSE
#' )
#'
#' @export
#'
#' @return Depends on the value of `hdi`.
#'
#'  * If `hdi = TRUE`, a `numeric vector` reporting the upper and lower limit
#'   of the hdi (attributes provide more detail on `cred_mass` and the applied
#'   sapwood model (`sw_data`)).
#'  * If `hdi = FALSE`, a `data.frame` with scaled p values for each number of
#'   observed sapwood rings.

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

    if (hdi == FALSE & plot == FALSE) {
      if (nrow(pdf) <= 1) {
        pdf[1, ] <- c(last, n_sapwood, NA)
        warning("--> No upper limit for the hdi could be computed.")
      }

      return(pdf)
    } else if (hdi == TRUE & nrow(pdf) <= 1) {
      # when a very high number of swr is given, the pdf_matrix is empty
      # --> create hdi manually
      hdi_int <- c(last, NA_integer_, NA_integer_)
      names(hdi_int) <- c("lower", "upper", "p")
      attr(hdi_int, "cred_mass") <- cred_mass
      attr(hdi_int, "sapwood_data") <- sw_data
      attr(hdi_int, "model") <- densfun

      warning("--> No upper limit for the hdi could be computed.")

      return(hdi_int)
    } else if (plot == TRUE) {
      sw_interval_plot(x = pdf)
    } else {
      attr(hdi_int, "cred_mass") <- cred_mass
      attr(hdi_int, "sapwood_data") <- sw_data
      attr(hdi_int, "model") <- densfun

      return(hdi_int)
    }
  }
