#' Plot a dated tree-ring series with a reference series or chronology
#'
#' Creates a line plot of a dated tree-ring series and a reference chronology,
#' with optional z-score standardization, statistical annotations, and visual highlighting
#' of synchronous growth changes (SCG).
#'
#' @param x A `data.frame` (typically of class `'rwl'`) with one column of ring-width values.
#'   Row names must be numeric and represent calendar years.
#' @param y A `data.frame` (typically of class `'rwl'`) with one column of ring-width values.
#'   Row names must be numeric and represent calendar years.
#' @param end_year Optional numeric. If provided, the rownames of `x` will be set to match this year for the last ring (using `trs_end_date()`).
#' @param zscore Logical. If `TRUE`, the ring-width series are standardized to z-scores before plotting.
#'   Defaults to `TRUE`.
#' @param pv_highlight Logical. If `TRUE`, highlights regions of parallel variation (synchronous growth change - sgc) using shaded bars.
#'   Defaults to `TRUE`.
#' @param pv_alpha Numeric between 0 and 1. Controls the transparency of the parallel variation highlight.
#'   Defaults to `0.2`.
#' @param show_stats Logical. If `TRUE`, displays crossdating statistics (e.g., overlap, correlation, sgc, t-values)
#'   as a plot subtitle. Defaults to `TRUE`.
#' @param labels Logical. If `TRUE`, displays year labels and points at the start and end positions.
#'   If `FALSE`, neither labels nor points are shown. Defaults to `TRUE`.
#' @param label_size Numeric. Controls the size of the year labels. Defaults to `3`.
#'
#' @return A `ggplot` object showing the plotted time series.
#'
#' @details
#' The function assumes that `x` and `y` are already calendar-dated and aligned by row names.
#' It performs internal trimming and optional z-scoring before plotting. Crossdating statistics
#' are computed using `trs_crossdate()`, and regions of synchronous growth change (SGC) are highlighted.
#' Required helper functions include: \code{\link{trs_trim}}, \code{\link{trs_zscore}},
#' \code{\link{trs_end_date}}, and \code{\link{sgc_for_plot}}. Statistics are derived from \code{\link{trs_crossdate}}.
#'
#' Required packages: **ggplot2**, **ggtext**, **plyr**
#'
#' @import ggplot2
#' @import ggtext
#' @importFrom plyr round_any
#'
#' @export
#'
#' @examples
#' x <- trs_pseudo_rwl(n_series = 1, series_length = 80, end_date = 1500, prefix = "trs_")
#' y <- trs_pseudo_rwl(n_series = 1, series_length = 400, end_date = 1700, prefix = "ref_")
#' trs_plot_dated(x, y)
#'
trs_plot_dated <- function(x, y,
                           end_year = NULL,
                           zscore = TRUE,
                           pv_highlight = TRUE,
                           pv_alpha = 0.2,
                           show_stats = TRUE,
                           labels = TRUE,
                           label_size = 3) {
     .data <- pv <- year <- label <- hjust <- NULL

     check_single_series(x, "x")
     check_single_series(y, "y")

     if (!is.null(end_year) && is.numeric(end_year)) {
          x <- trs_end_date(x, end_year = end_year, trim = TRUE)
     }
     if (!is.numeric(label_size) || label_size <= 0) {
          stop("label_size must be a positive number")
     }

     trs_name <- names(x)
     trs_plot <- trs_trim(x, rownames_to_years = TRUE)
     ref_name <- names(y)
     ref_plot <- trs_trim(y, rownames_to_years = TRUE)
     common_years <- intersect(trs_plot$year, ref_plot$year)
     years_min <- min(trs_plot$year)
     years_max <- max(trs_plot$year)

     ref_plot <- subset(ref_plot, year %in% c((years_min - 10):(years_max + 10)))

     if (length(common_years) < 10) {
          stop("overlap less than 10 years")
     }

     data_corr <- suppressMessages(
          trs_crossdate(
               x = trs_plot[, 1, drop = FALSE],
               y = ref_plot[, 1, drop = FALSE],
               min_overlap = 10,
               sliding = FALSE,
               pb = FALSE
          )
     )

     pv <- sgc_for_plot(trs_plot[, 1, drop = FALSE], ref_plot[, 1, drop = FALSE])
     pv$year <- as.numeric(rownames(pv))

     if (zscore) {
          trs_plot <- trs_zscore(trs_plot[, 1, drop = FALSE])
          trs_plot$year <- as.numeric(rownames(trs_plot))

          ref_plot <- trs_zscore(ref_plot[, 1, drop = FALSE])
          ref_plot$year <- as.numeric(rownames(ref_plot))
     }

     y_last <- trs_plot[trs_plot$year == years_max, 1]
     y_first <- trs_plot[trs_plot$year == years_min, 1]
     value_min <- min(c(trs_plot[[1]], ref_plot[[1]]))
     value_max <- max(c(trs_plot[[1]], ref_plot[[1]]))

     if (labels) {
          # Create data frame for year labels
          label_data <- data.frame(
               x = c(years_min - 2, years_max + 2),
               y = c(y_first, y_last),
               label = c(as.character(years_min), as.character(years_max)),
               hjust = c(1, 0)
          )

          # Create data frame for points
          point_data <- data.frame(
               x = c(years_min, years_max),
               y = c(y_first, y_last)
          )
     }

     plot <-
          ggplot2::ggplot() +
          {
               if (pv_highlight) {
                    ggplot2::geom_rect(
                         data = pv,
                         ggplot2::aes(
                              xmin = .data$year - 1,
                              xmax = .data$year,
                              ymin = value_min,
                              ymax = value_max,
                              fill = .data$sgc_logi
                         ),
                         color = NA,
                         alpha = pv_alpha
                    )
               }
          } +
          ggplot2::geom_line(
               data = trs_plot,
               ggplot2::aes(x = year, y = .data[[trs_name]])
          ) +
          ggplot2::geom_line(
               data = ref_plot,
               ggplot2::aes(
                    x = year,
                    y = .data[[ref_name]]
               ), color = "red3"
          ) +
          {
               if (labels) {
                    ggplot2::geom_point(
                         data = point_data,
                         ggplot2::aes(x = x, y = y),
                         shape = 21,
                         fill = "white",
                         size = 2
                    )
               }
          } +
          {
               if (labels) {
                    ggplot2::geom_label(
                         data = label_data,
                         ggplot2::aes(x = x, y = y, label = label, hjust = hjust),
                         size = label_size,
                         linewidth = 0,
                         fill = "white",
                         alpha = 0.8
                    )
               }
          } +
          ggplot2::scale_fill_manual(
               values = c("TRUE" = "#999999"),
               na.value = NA
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
               legend.position = "none",
               plot.title = ggtext::element_markdown(size = 10),
               plot.title.position = "plot",
               plot.caption = ggtext::element_markdown(size = 8),
               plot.subtitle = ggtext::element_markdown(face = "italic", size = 8)
          ) +
          ggplot2::scale_x_continuous(
               limits = c(years_min - 10, years_max + 10),
               breaks = seq(
                    plyr::round_any(years_min -
                         10, 10, f = floor),
                    plyr::round_any(years_max + 10, 10, f = floor),
                    by = 10
               )
          ) +
          ggplot2::scale_y_continuous(expand = c(0, 0)) +
          ggplot2::ylab(if (zscore) "z-score\n" else "ring width\n") +
          ggplot2::labs(
               title = paste(
                    data_corr["series"],
                    "<span style = 'color:white;'></span>",
                    "---",
                    "<span style = 'color:red3;'>",
                    data_corr["reference"],
                    "</span>"
               )
          ) +
          {
               if (show_stats) {
                    ggplot2::labs(
                         subtitle = paste0(
                              "ovl = ",
                              data_corr["overlap"],
                              "<span style = 'color:white;'>----</span>",
                              "r = ",
                              round(data_corr["r_pearson"], 2),
                              "<span style = 'color:white;'>----</span>",
                              "sgc = ",
                              data_corr["sgc"], "%",
                              "<span style = 'color:white;'>----</span>",
                              "t<sub>BP</sub> = ",
                              data_corr["t_BP"],
                              "<span style = 'color:white;'>----</span>",
                              "t<sub>Ho</sub> = ",
                              data_corr["t_Ho"]
                         )
                    )
               }
          }
     suppressWarnings(plot)
}
