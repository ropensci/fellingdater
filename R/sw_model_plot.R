#' Plot function for the output of [sw_model()]
#'
#' Returns a ggplot-style graph of the probability density function modelled to
#'  a data set of observed sapwood numbers, as computed by [sw_model()].
#'
#' @param x Output of [sw_model()].
#' @param bar_fill Fill color for the bars (original data).
#' @param bar_color Line color for the bars (original data).
#' @param line_color Line color for the fitted distribution.
#'
#' @return  A ggplot-style graph.
#' @examples
#' tmp <- sw_model(
#'   sw_data = "Hollstein_1980",
#'   densfun = "lognormal",
#'   cred_mass = .95,
#'   plot = FALSE
#' )
#' sw_model_plot(tmp,
#'               bar_fill = "forestgreen",
#'               bar_color = "darkgreen",
#'               line_color = "tomato3")
#'
#' @export
#'

sw_model_plot <-
        function(x,
                 bar_fill = "steelblue3",
                 bar_color = "grey60",
                 line_color = "red3"
                 ) {
  if (!all(names(x) %in% c(
    "sapwood_data",
    "n",
    "range",
    "density_function",
    "fit_parameters",
    "sapwood_model",
    "hdi_model"
  ))
  ) {
    stop("Input differs from output sw_model()")
  }

  # to avoid notes in CMD check
  count <- n_sapwood <- y <- NULL

  max <- x$range[3]
  hdi_lower <- x$hdi_model[[1]]
  hdi_upper <- x$hdi_model[[2]]
  spline_int <-
    as.data.frame(stats::spline(
      x$sapwood_model$n_sapwood,
      x$sapwood_model$model_fit,
      # xout = seq(1, max, 0.2)
      xout = seq(1, 100, 0.2)
    ))
  spline_int <- subset(spline_int, y > 1e-02)

  p <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = x$sapwood_model,
      ggplot2::aes(
        x = n_sapwood,
        y = count
      ),
      fill = bar_fill,
      color = bar_color,
      alpha = .4
    ) +
    ggplot2::geom_ribbon(
      data = subset(
        spline_int,
        x >= hdi_lower &
          x <= hdi_upper
      ),
      ggplot2::aes(
        ymin = 0,
        ymax = y,
        x = x
      ),
      fill = "grey30",
      alpha = 0.2
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = hdi_lower,
        xend = hdi_upper,
        y = 0,
        yend = 0
      ),
      colour = "grey30",
      linewidth = .8,
      alpha = 0.8
    ) +
    ggplot2::geom_line(
      data = spline_int,
      ggplot2::aes(x = x, y = y),
      linewidth = 1,
      color = line_color
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    ggplot2::labs(
      subtitle = paste0(
        "sapwood data: ", x$sapwood_data, "<br>",
        "<span style='color:", bar_fill, "'> original data</span> (n = ",
        x$n,
        ")",
        "<br> <span style='color:", line_color,
        "'>  fitted distribution</span> (",
        x$density_function, ")", "<br> <span style='color:grey30'>hdi (",
        round(x$hdi_model$p * 100, 1), "%)</span>: __between ",
        hdi_lower,
        " and ",
        hdi_upper,
        " sapwood rings__"
      )
    ) +
    ggplot2::xlab("number of sapwood rings") +
    ggplot2::ylab("n\n") +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(),
      plot.subtitle = ggtext::element_markdown(hjust = 0),
      plot.title.position = "plot"
    )
  suppressWarnings(print(p))
}
