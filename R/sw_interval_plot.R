#' Plot function for the output of [sw_interval()]
#'
#' Returns a ggplot-style graph of the probability density function for the
#' felling date range, as computed by [sw_interval()].
#'
#' @param x Output of [sw_interval()].
#' @param area_fill Fill color for the area under fitted distribution.
#' @param line_color Line color for the fitted distribution.
#'
#' @return  A ggplot-style graph.
#' @examples
#' tmp <- sw_interval(
#'   n_sapwood = 10,
#'   last = 1000,
#'   hdi = FALSE,
#'   cred_mass = .95,
#'   sw_data = "Hollstein_1980",
#'   densfun = "lognormal",
#'   plot = FALSE
#' )
#' sw_interval_plot(tmp)
#'
#' @export
#'
sw_interval_plot <-
        function(x,
                 area_fill = "tomato3",
                 line_color = "tomato3") {
  if (all(
    !(attributes(x)$names) %in% c(
      "year",
      "n_sapwood",
      "p"
    )
  )) {
    stop("Input differs from output sw_interval()")
  }
  # to avoid notes in CMD check
  p.x <- upper <- year <- NULL

  sw_data_p <- attributes(x)$sapwood_data
  densfun_p <- attributes(x)$model
  hdi_p <- attributes(x)$hdi
  cred_mass_p <- attributes(x)$cred_mass

  p_model <- sw_model(
    sw_data = sw_data_p,
    densfun = densfun_p,
    cred_mass = cred_mass_p,
    plot = FALSE
  )$sapwood_model

  lower <- x[[1, 1]]
  n_sapwood <- x[[1, 2]]

  df <- merge(p_model, x, by = "n_sapwood", all = TRUE)
  max_p <- max(df$p.x, na.rm = TRUE)
  end <- max(df$year, na.rm = TRUE)
  start <- end - nrow(df) + 1
  years <- seq(start, end, 1)
  df[, "year"] <- years
  range <- range(df$year)

  p <- ggplot2::ggplot(data = df) +
    ggplot2::geom_line(
      ggplot2::aes(x = year, y = p.x),
      color = "grey40",
      linewidth = 0.05
    ) +
    ggplot2::geom_area(
      ggplot2::aes(
        x = ifelse(year >= lower, year, NA),
        y = p.x
      ),
      fill = area_fill,
      color = line_color,
      alpha = 0.3,
      linewidth = 1
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(
      limits = c(
        floor(range[[1]] / 10) * 10,
        ceiling(range[[2]] / 10) * 10
      ),
      breaks = seq(
        floor(range[[1]] / 10) * 10,
        ceiling(range[[2]] / 10) * 10,
        10
      )
    ) +
    ggplot2::geom_segment(
      data = hdi_p,
      ggplot2::aes(
        x = lower,
        xend = upper,
        y = 0,
        yend = 0
      ),
      linewidth = .8
    ) +
    ggplot2::annotate(
      "text",
      x = end,
      y = max_p,
      label = paste0(
        "date last measured ring: ",
        lower,
        "\nmeasured sapwood rings: ",
        n_sapwood,
        "\nsapwood data: ",
        sw_data_p,
        "\nmodel: ",
        densfun_p,
        "\nhdi: ",
        hdi_p[[1]],
        " - ",
        hdi_p[[2]],
        "\ncred. mass: ",
        100 * cred_mass_p,
        "%"
      ),
      hjust = 1,
      vjust = 1
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 10),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "none",
      strip.background = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank()
    ) +
    ggplot2::ylab("p") +
    ggplot2::xlab("calendar year")

  suppressWarnings(print(p))
}
