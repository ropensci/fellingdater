#' Plot all series in a tree-ring width dataframe
#'
#' Creates a line plot of all tree-ring series in an rwl dataframe,
#' with optional z-score standardization and faceting options.
#'
#' @param x A `data.frame` (typically of class `'rwl'`) with multiple columns of ring-width values.
#'   Row names must be numeric and represent calendar years.
#' @param zscore Logical. If `TRUE`, the ring-width series are standardized to z-scores before plotting.
#'   Defaults to `TRUE`.
#' @param facet Logical. If `TRUE`, creates a faceted plot with each series in a separate panel.
#'   If `FALSE`, plots all series overlaid on the same plot. Defaults to `FALSE`.
#' @param ncol Numeric. Number of columns for faceted plots (only used when `facet = TRUE`).
#'   Defaults to `1`.
#' @param color Character string or vector specifying line color(s). When `NULL` and `facet = FALSE`,
#'   each series gets a different color. When a single color is specified, all lines use that color.
#'   When a vector of colors is provided, each series gets the corresponding color. Defaults to `NULL`.
#' @param scale_y Character string specifying y-axis scaling for faceted plots.
#'   Options are "fixed", "free", "free_x", or "free_y". Defaults to `"fixed"`.
#'
#' @return A `ggplot` object showing the plotted time series.
#'
#' @details
#' The function assumes that the rwl dataframe is already calendar-dated with years as row names.
#' When `facet = FALSE`, all series are plotted on the same axes with different colors.
#' When `facet = TRUE`, each series gets its own panel for easier individual inspection.
#' When providing a vector of colors, the length must match the number of series in the data.
#' Required helper functions include: \code{\link{trs_trim}} and \code{\link{trs_zscore}}.
#'
#' Required packages: **ggplot2**, **plyr**, **tidyr**, **dplyr**
#'
#' @import ggplot2
#' @importFrom plyr round_any
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr group_by summarise n left_join mutate
#'
#' @export
#'
#' @examples
#' rwl_data <- trs_pseudo_rwl(n_series = 5, series_length = c(75, 100), end_date = c(2000, 2025))
#'
#' # Overlaid plot with z-scores
#' trs_plot_rwl(rwl_data, zscore = TRUE, facet = FALSE)
#'
#' # Faceted plot with raw values
#' trs_plot_rwl(rwl_data, zscore = FALSE, facet = TRUE, ncol = 1)
#'
#' # Custom colors for each series
#' col_vec <- c("#0fa3b1", "#07beb8", "#3dccc7", "#68d8d6", "#9ceaef")
#' trs_plot_rwl(rwl_data, color = c("tomato3", "navyblue", "forestgreen", "purple", "orange"))
#' trs_plot_rwl(rwl_data, color = col_vec, zscore = FALSE)
#'
trs_plot_rwl <- function(x,
                         zscore = TRUE,
                         facet = FALSE,
                         ncol = 1,
                         color = NULL,
                         scale_y = "fixed") {
  .data <- series <- value <- year <- series_label <- length_series <- year_first <- year_last <- NULL

  check_consecutive(x)

  # Check if rwl is a dataframe
  if (!is.data.frame(x)) {
    stop("rwl must be a data.frame")
  }

  # Check if rwl has any columns
  if (length(colnames(x)) == 0) {
    stop("rwl must have at least one column")
  }

  # Prepare the data
  rwl_plot <- trs_trim(x, rownames_to_years = TRUE)

  # Apply z-score transformation if requested
  if (zscore) {
    # Apply z-score to each series column (excluding year column)
    series_cols <- names(rwl_plot)[names(rwl_plot) != "year"]
    for (col in series_cols) {
      rwl_plot[[col]] <- trs_zscore(rwl_plot[col])[[1]]
    }
  }

  # Convert to long format for ggplot
  rwl_long <- tidyr::pivot_longer(
    rwl_plot,
    cols = -year,
    names_to = "series",
    values_to = "value"
  )

  # Remove NA values
  rwl_long <- rwl_long[!is.na(rwl_long$value), ]

  # Check color vector length if provided
  n_series <- length(unique(rwl_long$series))
  if (!is.null(color) && length(color) > 1) {
    if (length(color) != n_series) {
      stop(paste0(
        "Length of color vector (", length(color),
        ") must match number of series (", n_series, ")"
      ))
    }
  }

  # Create enhanced series labels for facets if needed
  if (facet) {
    series_info <- rwl_long |>
      dplyr::group_by(series) |>
      dplyr::summarise(
        year_first = min(year, na.rm = TRUE),
        year_last = max(year, na.rm = TRUE),
        length_series = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        series_label = paste0(series, ": ", year_first, " - ", year_last, " (n = ", length_series, ")")
      )

    # Add the enhanced labels to the main data
    rwl_long <- rwl_long |>
      dplyr::left_join(series_info[, c("series", "series_label")], by = "series")
  }

  # Calculate year range for x-axis
  years_min <- min(rwl_long$year, na.rm = TRUE)
  years_max <- max(rwl_long$year, na.rm = TRUE)

  # Create base plot
  plot <- ggplot2::ggplot(
    rwl_long,
    ggplot2::aes(x = year, y = value)
  )

  # Add lines based on faceting option
  if (facet) {
    # For faceted plots, determine colors
    if (is.null(color)) {
      # Default single color for all facets
      plot <- plot +
        ggplot2::geom_line(color = "grey30") +
        ggplot2::facet_wrap(~series_label, ncol = ncol, scales = scale_y)
    } else if (length(color) == 1) {
      # Single color specified for all facets
      plot <- plot +
        ggplot2::geom_line(color = color) +
        ggplot2::facet_wrap(~series_label, ncol = ncol, scales = scale_y)
    } else {
      # Vector of colors - different color per facet
      plot <- plot +
        ggplot2::geom_line(ggplot2::aes(color = series)) +
        ggplot2::facet_wrap(~series_label, ncol = ncol, scales = scale_y) +
        ggplot2::scale_color_manual(values = color)
    }
  } else {
    # For overlaid plots
    if (is.null(color)) {
      # Default: different colors for each series (automatic ggplot colors)
      plot <- plot +
        ggplot2::geom_line(ggplot2::aes(color = series))
    } else if (length(color) == 1) {
      # Single color for all series
      plot <- plot +
        ggplot2::geom_line(ggplot2::aes(group = series), color = color)
    } else {
      # Vector of colors - different color per series
      plot <- plot +
        ggplot2::geom_line(ggplot2::aes(color = series)) +
        ggplot2::scale_color_manual(values = color)
    }
  }

  # Apply theme and formatting (theme_minimal first, then custom strip text)
  plot <- plot +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(
      breaks = seq(
        plyr::round_any(years_min, 10, f = floor),
        plyr::round_any(years_max, 10, f = ceiling),
        by = 10
      )
    ) +
    ggplot2::ylab(if (zscore) "z-score\n" else "ring width\n") +
    ggplot2::xlab("year") +
    ggplot2::labs(color = "") +
    # Apply strip text alignment AFTER theme_minimal to avoid overwriting
    ggplot2::theme(strip.text.x = ggplot2::element_text(hjust = 0)) +
    ggplot2::theme(legend.position = "none")


  # Adjust legend position for non-faceted plots
  if (!facet) {
    plot <- plot + ggplot2::theme(legend.position = "right")
  }

  return(plot)
}
