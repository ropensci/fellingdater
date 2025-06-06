#' Plot the output of [sw_sum()]
#'
#' @description This function creates a visualization of the summed probability
#'   density (SPD) output from [sw_sum()]. It displays both the SPD as a bar plot
#'   and exact felling dates (waney edge series) as stacked points. A smoothing
#'   spline is also added to reveal long-term trends in felling activity.
#'
#'
#' @param x A `data.frame`, typically the output of [sw_sum()] with `plot = FALSE`.
#'   Must contain columns `"year"` and `"spd"`. If available, `"spd_wk"` (waney edge counts)
#'   will be used to add symbols representing exact felling years.
#' @param bar_col Fill color for the SPD bars. Default is `"steelblue"`.
#' @param trend_col Color of the smoothing spline line.
#' @param dot_col Fill color of the symbols representing exact felling dates (waney edge).
#'   Default is `"steelblue4"`.
#' @param dot_size Size of the felling date symbols. Default is `2`.
#' @param dot_shape Shape code for the felling date symbols. See `?points` for options.
#'   Default is `21` (circle).
#' @param window_smooth Numeric value specifying the smoothing window width (in years)
#'   for calculating the moving average trend line. Default is `11`.
##'
#' @return A `ggplot` object showing:
#' - The SPD as a bar plot.
#' - A smoothing spline through the SPD.
#' - Stacked symbols for exact felling years (waney edge), if available.
#'
#' @seealso [sw_sum()] to generate the SPD data.
#'
#' @export
#'
#' @examples
#' sw_example6
#' tmp <- sw_sum(sw_example6, plot = FALSE)
#' sw_sum_plot(tmp,
#'      bar_col = "burlywood1",
#'      trend_col = "brown",
#'      dot_col = "orange",
#'      dot_shape = 23, dot_size = 5
#' )
#'
sw_sum_plot <- function(x,
                        bar_col = "steelblue",
                        trend_col = "red3",
                        dot_col = "steelblue4",
                        dot_size = 2,
                        dot_shape = 21,
                        window_smooth = 11) {
     # to avoid notes in CMD check
     spd_wk <- year <- y <- spd <- spd_mov_av <- .data <- NULL


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
          ggplot2::geom_col(ggplot2::aes(x = .data$year, y = .data$spd),
               fill = bar_col,
               alpha = 0.8,
               na.rm = TRUE
          ) +
          ggplot2::geom_line(
               data = spline_int,
               ggplot2::aes(x = .data$x, y = .data$y),
               color = trend_col,
               linewidth = 1.5,
               alpha = .8,
               na.rm = TRUE
          ) +
          {
               if (nrow(fd) > 0) {
                    ggplot2::geom_point(
                         data = fd,
                         # jittering points vertically based on number of exact dates
                         ggplot2::aes(x = .data$year, y = .data$spd + .data$y * p_max / 20),
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
          theme_fdr() +
          ggplot2::xlab("calendar year")
     return(p)
}
