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
#' @param x_breaks Numeric. The interval between x-axis tick marks (in years).
#'   If `NULL` (default), the interval is chosen automatically based on the
#'   length of the series. Must be a positive number if provided.
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
trs_plot_dated <- function(
     x,
     y,
     end_year = NULL,
     zscore = TRUE,
     pv_highlight = TRUE,
     pv_alpha = 0.2,
     show_stats = TRUE,
     labels = TRUE,
     label_size = 3,
     x_breaks = NULL
) {
     .data <- pv <- year <- label <- hjust <- NULL

     check_single_series(x, "x")
     check_single_series(y, "y")

     if (!is.null(end_year)) {
          if (!is.numeric(end_year)) {
               stop("'end_year' must be a numeric value")
          }
          x <- trs_end_date(x, end_year = end_year, trim = TRUE)
     }

     if (!is.numeric(label_size) || label_size <= 0) {
          stop("label_size must be a positive number")
     }

     trs_name_display <- names(x)
     names(x) <- make.names(names(x))
     trs_name <- names(x)
     trs_plot <- trs_trim(x, rownames_to_years = TRUE)
     ref_name_display <- names(y)
     names(y) <- make.names(names(y))
     ref_name <- names(y)
     ref_plot <- trs_trim(y, rownames_to_years = TRUE)
     common_years <- intersect(trs_plot$year, ref_plot$year)
     years_min <- min(trs_plot$year)
     years_max <- max(trs_plot$year)

     ref_plot <- subset(
          ref_plot,
          year %in% c((years_min - 10):(years_max + 10))
     )

     if (length(common_years) < 10) {
          stop("overlap less than 10 years")
     }

     data_corr <- suppressMessages(
          trs_crossdate(
               x = trs_plot[, trs_name, drop = FALSE],
               y = ref_plot[, ref_name, drop = FALSE],
               min_overlap = 10,
               sliding = FALSE,
               pb = FALSE
          )
     )

     pv <- sgc_for_plot(
          trs_plot[, trs_name, drop = FALSE],
          ref_plot[, ref_name, drop = FALSE]
     )
     pv$year <- as.numeric(rownames(pv))

     if (zscore) {
          trs_plot <- trs_zscore(trs_plot[, trs_name, drop = FALSE])
          trs_plot$year <- as.numeric(rownames(trs_plot))

          ref_plot <- trs_zscore(ref_plot[, ref_name, drop = FALSE])
          ref_plot$year <- as.numeric(rownames(ref_plot))
     }

     y_last <- trs_plot[trs_plot$year == years_max, trs_name]
     y_first <- trs_plot[trs_plot$year == years_min, trs_name]
     # to avoid labels get clipped, with expand = c(0, 0) in scale_y_continuous
     value_min <- floor(
          min(c(trs_plot[[trs_name]], ref_plot[[ref_name]]), na.rm = TRUE) * 4
     ) /
          4
     value_max <- ceiling(
          max(c(trs_plot[[trs_name]], ref_plot[[ref_name]]), na.rm = TRUE) * 4
     ) /
          4

     # Auto-select break interval if not specified
     if (!is.null(x_breaks) && (!is.numeric(x_breaks) || x_breaks <= 0)) {
          stop("'x_breaks' must be a positive number")
     }
     if (is.null(x_breaks)) {
          series_length <- years_max - years_min
          x_breaks <- dplyr::case_when(
               series_length <= 100 ~ 10,
               series_length <= 250 ~ 20,
               series_length <= 500 ~ 50,
               TRUE ~ 100
          )
     }

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
          {
               if (zscore) {
                    ggplot2::geom_hline(
                         yintercept = 0,
                         linetype = "dashed",
                         alpha = 0.3
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
               ),
               color = "red3"
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
                         ggplot2::aes(
                              x = x,
                              y = y,
                              label = label,
                              hjust = hjust
                         ),
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
               plot.subtitle = ggtext::element_markdown(
                    face = "italic",
                    size = 8
               )
          ) +
          ggplot2::scale_x_continuous(
               limits = c(years_min - 10, years_max + 10),
               breaks = seq(
                    plyr::round_any(years_min - 10, x_breaks, f = floor),
                    plyr::round_any(years_max + 10, x_breaks, f = floor),
                    by = x_breaks
               )
          ) +
          ggplot2::scale_y_continuous(expand = c(0, 0)) +
          ggplot2::ylab(if (zscore) "z-score\n" else "ring width\n") +
          ggplot2::labs(
               title = paste(
                    trs_name_display,
                    "<span style = 'color:white;'></span>",
                    "---",
                    "<span style = 'color:red3;'>",
                    ref_name_display,
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
                              data_corr["sgc"],
                              "%",
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
