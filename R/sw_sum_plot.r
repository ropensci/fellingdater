#' Plot the output of [sw_sum()]
#'
#' @description This function plots the result of `sw_sum()` - both the SPD and
#'   the occurrence of exact felling dates - and adds a smoothing spline to the
#'   SPD.
#'
#' @param x The output of `sw_sum()`.
#' @param bar_col The fill color for the bars.
#' @param trend_col The line color of the fitted smoothing spline.
#' @param dot_col The color of the shapes that represent exact felling dates.
#' @param dot_size A size argument for the shapes that represent exact felling
#' dates.
#' @param dot_shape Number corresponding to the point symbol available in R for
#' the shapes that represent exact felling dates.
#' @param window_smooth A numeric value for the window width of the trend line
#'
#' @return A ggplot style graph, with calendar years on the X-axis and the
#' probability (p) on the Y-axis.
#' @export
#'
#' @examples
#' trs_example6 <- trs_example6
#' trs_example6
#' tmp <- sw_sum(trs_example6)
#' sw_sum_plot(tmp, bar_col = "burlywood1",
#'                  trend_col = "brown",
#'                  dot_col = "orange",
#'                  dot_shape = 23, dot_size = 5)
#'
sw_sum_plot <- function(x,
                        bar_col = "steelblue",
                        trend_col = "red3",
                        dot_col = "steelblue4",
                        dot_size = 2,
                        dot_shape = 21,
                        window_smooth = 11) {
  # to avoid notes in CMD check
  spline <- spd_wk <- year <- y <- spd <- spd_mov_av <- NULL


  pdf_matrix <- as.data.frame(x)

  smooth <-
    as.data.frame(
      mov_av(pdf_matrix$spd, window_smooth, align = "center", edges = "fill")
    )
  names(smooth) <- "spd_mov_av"

  pdf_matrix <- cbind(pdf_matrix, smooth)

  spline_int <-
    as.data.frame(
      spline(pdf_matrix$year, pdf_matrix$spd_mov_av, method = "natural")
    )

  plot_range <- range(pdf_matrix$year)

  p_max <- max(pdf_matrix$spd)

  fd <- pdf_matrix[, c("year", "spd_wk")]
  fd <- subset(fd, spd_wk > 0)
  if (nrow(fd) > 0) {
    fd <- rep(fd$year, fd$spd_wk)
    fd <- data.frame(fd)
    names(fd) <- "year"
    fd$y <- 1
    fd <- fd |>
      dplyr::group_by(year) |>
      dplyr::mutate(y = cumsum(y)) |>
      dplyr::left_join(pdf_matrix |> dplyr::select("year", "spd"), by = "year")
  }

  p <-
    pdf_matrix |>
    dplyr::select(year, spd, spd_mov_av) |>
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = year, y = spd),
      fill = bar_col,
      alpha = 0.8,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = spline_int,
      ggplot2::aes(x = x, y = y),
      color = trend_col,
      linewidth = 1.5,
      alpha = .8,
      na.rm = TRUE
    ) +
    {
      if (nrow(fd) > 0) {
        ggplot2::geom_point(
          data = fd,
          ggplot2::aes(x = year, y = spd + y * p_max / 20),
          size = dot_size,
          fill = dot_col,
          color = dot_col,
          shape = dot_shape,
          na.rm = TRUE
        )
      }
    } +
    ggplot2::scale_x_continuous(
      limits = c(
        floor(plot_range[1] / 10) * 10,
        ceiling(plot_range[2] / 10) * 10
      ),
      breaks = seq(
        floor(plot_range[1] / 10) * 10,
        ceiling(plot_range[2] / 10) * 10,
        20
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::xlab("calendar year") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 10),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "none"
    )
  return(p)
}
