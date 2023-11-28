#' Plot the output of [sw_sum()]
#'
#' @description This function plots the result of `sw_sum()` - both the SPD and
#'   the occurrence of exact felling dates - and adds a smoothing spline to the
#'   SPD.
#'
#' @param x The output of `sw_sum()`.
#' @param bar_col The fill color for the bars.
#' @param spline_col The line color of the fitted smoothing spline.
#' @param dot_col The color of the shapes that represent exact felling dates.
#' @param dot_size A size argument for the shapes that represent exact felling dates.
#' @param dot_shape Number corresponding to the point symbol available in R for the shapes that represent exact felling dates.
#'
#' @return A ggplot style graph.
#' @export
#'
#' @examples
#' dummy6 <- fellingdateR:::dummy6
#' dummy6
#' tmp <- sw_sum(dummy6)
#' sw_sum_plot(tmp, bar_col = "burlywood1")
#'
sw_sum_plot <- function(x,
                        bar_col = "steelblue",
                        spline_col = "red3",
                        dot_col = "steelblue4",
                        dot_size = 2,
                        dot_shape = 21) {

        # to avoid notes in CMD check
        spline <- SPD_wk <- year <- y <- SPD <- SPD_movAv <- NULL


pdf_matrix <- as.data.frame(x)

smooth <-
        as.data.frame(
                movAv(pdf_matrix$SPD, 11, align = "center", edges = "fill")
                )
names(smooth) <- "SPD_movAv"

pdf_matrix <- cbind(pdf_matrix, smooth)

spline_int <-
        as.data.frame(
                spline(pdf_matrix$year, pdf_matrix$SPD_movAv, method = "natural")
                )

plot_range <- range(pdf_matrix$year)

p_max <- max(pdf_matrix$SPD)

fd <- pdf_matrix[, c("year", "SPD_wk")]
fd <- subset(fd, SPD_wk>0)
if (nrow(fd) > 0){
        fd <- rep(fd$year, fd$SPD_wk)
        fd <- data.frame(fd)
        names(fd) <- "year"
        fd$y <- 1
        fd <- fd |>
        dplyr::group_by(year) |>
        dplyr::mutate(y = cumsum(y)) |>
        dplyr::left_join(pdf_matrix |> dplyr::select("year", "SPD"), by = "year")
}

p <-
     pdf_matrix |>
        dplyr::select(year, SPD, SPD_movAv) |>
        ggplot2::ggplot() +
        ggplot2::geom_col(ggplot2::aes(x = year, y = SPD),
                          fill = bar_col,
                          alpha = 0.8,
                          na.rm = T) +
        ggplot2::geom_line(data = spline_int,
                           ggplot2::aes(x = x, y = y),
                           color = spline_col,
                           linewidth = 1.5,
                           alpha = .8,
                           na.rm = T) +
        { if (nrow(fd) > 0) ggplot2::geom_point(data = fd,
                            ggplot2::aes(x = year, y = SPD + y * p_max/20),
                            size = dot_size,
                            fill = dot_col,
                            color = dot_col,
                            shape = dot_shape,
                            na.rm = T) } +
        ggplot2::scale_x_continuous(
                limits = c(floor(plot_range[1]/10)*10,
                           ceiling(plot_range[2]/10)*10),
                breaks = seq(floor(plot_range[1]/10)*10,
                             ceiling(plot_range[2]/10)*10,
                             20)) +
        ggplot2::theme_minimal()  +
        ggplot2::xlab("calendar year") +
        ggplot2::theme(
                axis.text=ggplot2::element_text(size=10),
                panel.grid.minor.y=ggplot2::element_blank(),
                panel.grid.major.y=ggplot2::element_blank(),
                legend.position = "none"
                )
return(p)
}
