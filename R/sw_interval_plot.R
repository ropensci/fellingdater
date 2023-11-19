#' Plot function for the output of [sw_interval()]
#'
#' Returns a ggplot-style graph of the probability density function for the
#' felling date range, as computed by [sw_interval()].
#'
#' @param x Output of [sw_interval()].
#'
#' @return  A ggplot-style graph.
#' @examples
#' tmp <- sw_interval(n_sapwood = 10,
#'             last = 1000,
#'             hdi = FALSE,
#'             credMass = .95,
#'             sw_data = "Hollstein_1980",
#'             densfun = "lognormal",
#'             plot = FALSE)
#' sw_interval_plot(tmp)
#'
#' @export
#'
sw_interval_plot <- function(x) {
        # to avoid notes in CMD check
        p.x <- upper <- year <- NULL

        sw_data.p <- attributes(x)$sapwood_data
        densfun.p <- attributes(x)$model
        hdi.p <- attributes(x)$hdi
        credMass.p <- attributes(x)$credMass
        sep.p <- attributes(x)$sep

        p_model <- sw_model(
                sw_data = sw_data.p,
                densfun = densfun.p,
                credMass = credMass.p,
                sep = sep.p,
                plot = FALSE
        )$sapwood_model

        lower <- x[[1, 1]]
        n_sapwood <- x[[1, 2]]

        df <- merge(p_model, x, by = "n_sapwood", all = T)
        max_p <- max(df$p.x, na.rm = TRUE)
        end <- max(df$year, na.rm = TRUE)
        start <- end - nrow(df) + 1
        years <- seq(start, end, 1)
        df[, "year"] <- years
        range <- range(df$year)

        if (grepl("\\.csv$", sw_data.p))
                sw_data.p <- basename(sw_data.p)

        p <- ggplot2::ggplot(data = df) +

                ggplot2::geom_line(
                        ggplot2::aes(x = year, y = p.x),
                        color = "grey40",
                        linewidth = 0.05
                ) +
                ggplot2::geom_area(
                        ggplot2::aes(x = ifelse(year >= lower, year, NA),
                                     y = p.x),
                        fill = "tomato3",
                        color = "tomato3",
                        alpha = 0.3,
                        linewidth = 1
                )  +

                ggplot2::theme_minimal() +

                ggplot2::scale_x_continuous(
                        limits = c(
                                plyr::round_any(range[1], 10, floor),
                                plyr::round_any(range[2], 10, ceiling)
                        ),
                        breaks = seq(
                                plyr::round_any(range[1], 10, floor),
                                plyr::round_any(range[2], 10, ceiling),
                                10
                        )
                ) +

                ggplot2::geom_segment(
                        data = hdi.p,
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
                                sw_data.p,
                                "\nmodel: ",
                                densfun.p,
                                "\nhdi: ",
                                hdi.p[[1]],
                                " - ",
                                hdi.p[[2]],
                                "\ncredMass: ",
                                100 * credMass.p,
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
