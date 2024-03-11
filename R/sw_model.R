#' Model sapwood data and compute the highest posterior density interval
#'
#' @description This function fits a distribution to a data set of observed
#'   sapwood numbers and computes the highest posterior density interval (hdi)
#'   for a given credibility mass.
#'
#' @param sw_data The name of the sapwood data set to use for modelling. It
#'   should be one of the data sets listed in [sw_data_overview()], or the name
#'   of a `data.frame` with sapwood data in columns `n_sapwood` and `count`.
#' @param densfun Name of the density function to fit to the sapwood data set.
#'   Should be one of:
#'   * _lognormal_ (the default value),
#'   * _normal_,
#'   * _weibull_,
#'   * _gammma_.
#' @param cred_mass A `scalar` in the range of `[0, 1]` specifying the mass
#'   within the credible interval (default = .954).
#' @param plot A `logical`. If `TRUE` a plot of the fitted density function is
#'   returned. When `FALSE`, a list with numeric output of the modelling process
#'   is returned.
#' @param source A character string with info on the data source. This info is
#'   incorporated in the output when a sapwood data set is provided from a
#'   user-defined `data.frame`.
#'
#' @return The return value depends on the `plot` parameter.
#'   * if `plot` is TRUE, a ggplot-style graph is returned.
#'   * if `plot` is FALSE, a list with the numeric output of the modelling
#'   process is returned.
#'
#' @export
#' @examples
#' sw_model("Sohar_2012_ELL_c")
#'
#' sw_model("Hollstein_1980")
#'
sw_model <-
  function(sw_data = "Hollstein_1980",
           densfun = "lognormal",
           cred_mass = 0.954,
           source = NA,
           plot = FALSE) {
    if (is.na(cred_mass) || cred_mass <= 0 || cred_mass >= 1) {
      stop("--> cred_mass must be between 0 and 1")
    }

    if (!densfun %in% c("lognormal", "normal", "weibull", "gamma")) {
      stop(
        sprintf(
          "\n'%s' is not a supported distribution.
          \n`densfun` must be one of c('lognormal', 'normal', 'weibull', 'gamma')",
          densfun
        )
      )
    }

    if (!is.logical(plot)) {
      stop(sprintf(
        "'plot' should be TRUE or FALSE, not '%s'",
        plot
      ))
    }

    if (is.character(sw_data) && sw_data %in% sw_data_overview()) {
      observed <- get(sw_data)
      sw_source <- if (!is.na(source)) source else sw_data
    }

    if (is.character(sw_data) && !sw_data %in% sw_data_overview()) {
      observed <- get(sw_data)
      sw_source <- if (!is.na(source)) source else "user defined"
    }

    if (!is.character(sw_data)) {
      stop("--> sw_data should be one of `sw_data_overview()`
or the name a data.frame with numeric values in columns `n_sapwood` and `count`.)")
    }

    if (!all(c("n_sapwood", "count") %in% names(observed))) {
      stop("--> sw_data should be a data.frame with numeric values in columns
           `n_sapwood` and `count`.)")
    }

    if (!(is.numeric(observed$n_sapwood) && is.numeric(observed$count))) {
      stop(
        "--> sw_data should have numeric values in columns `n_sapwood` and `count`.)"
      )
    }

    observed <- observed[, c("n_sapwood", "count")]

    observed <- subset(observed, count > 0)
    n_obs <- sum(observed$count)
    min <- min(observed$n_sapwood)
    max <- max(observed$n_sapwood)
    mean <- round(mean(observed$n_sapwood), 2)
    range <- c(min, mean, max)
    names(range) <- c("min", "mean", "max")

    df <-
      data.frame(n_sapwood = rep(observed$n_sapwood, observed$count))

    fit_params <-
      MASS::fitdistr(df |> dplyr::pull(n_sapwood), densfun)

    sw_model <- data.frame(
      model_fit = d_count(
        densfun = densfun,
        x = rep(1:max(df$n_sapwood), 1),
        param1 = fit_params$estimate[[1]],
        param2 = fit_params$estimate[[2]],
        n = n_obs
      )
    )
    sw_model["n_sapwood"] <-
      as.numeric(rownames(sw_model))

    sw_model <- sw_model |>
      dplyr::mutate(p = model_fit / n_obs)

    sw_model <-
      merge(sw_model, observed, all.x = TRUE) |>
      dplyr::relocate(p, .after = n_sapwood)

    hdi_model <-
      hdi(
        x = sw_model,
        cred_mass = cred_mass
      )

    spline_int <-
      as.data.frame(stats::spline(
        sw_model$n_sapwood,
        sw_model$model_fit,
        xout = seq(1, max, 0.2)
      ))

    output <- list(
      sapwood_data = sw_source,
      n = n_obs,
      range = range,
      density_function = densfun,
      fit_parameters = fit_params,
      sapwood_model = sw_model,
      hdi_model = hdi_model
    )
    if (plot == TRUE) {
      sw_model_plot(output)
    } else {
      output
    }
  }


################################################################################
# helper function to scale a Prob. Density Function (PDF) to a
# Prob. Frequency Function.

d_count <- function(densfun = densfun,
                    x = x,
                    param1 = 0,
                    param2 = 1,
                    log = FALSE,
                    n = 1) {
  if (!densfun %in% c("lognormal", "normal", "weibull", "gamma")) {
    stop(sprintf(
      "!!! '%s' is not a supported distribution !!!",
      densfun
    ))
  }
  if (densfun == "lognormal") {
    n * stats::dlnorm(
      x = x,
      meanlog = param1,
      sdlog = param2,
      log = log
    )
  } else if (densfun == "normal") {
    n * stats::dnorm(
      x = x,
      mean = param1,
      sd = param2,
      log = log
    )
  } else if (densfun == "weibull") {
    n * stats::dweibull(
      x = x,
      shape = param1,
      scale = param2,
      log = log
    )
  } else if (densfun == "gamma") {
    n * stats::dgamma(
      x = x,
      shape = param1,
      rate = param2,
      log = log
    )
  }
}
