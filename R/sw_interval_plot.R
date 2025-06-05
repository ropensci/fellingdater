#' Plot function for the output of [sw_interval()]
#'
#' Returns a ggplot-style graph of the probability density function for the
#' felling date range, as computed by [sw_interval()].
#'
#' @param x Output of [sw_interval()] with `hdi = FALSE`.
#' @param area_fill Fill color for the area under fitted distribution.
#' @param line_color Line color for the fitted distribution.
#'
#' @return  A ggplot-style graph, with calendar years on the X-axis and the
#' probability (p) on the Y-axis.
#' @examples
#' tmp <- sw_interval(
#'      n_sapwood = 10,
#'      last = 1000,
#'      hdi = FALSE,
#'      cred_mass = .95,
#'      sw_data = "Hollstein_1980",
#'      densfun = "lognormal",
#'      plot = FALSE
#' )
#' sw_interval_plot(tmp, area_fill = "forestgreen", line_color = "forestgreen")
#'
#' @export
#'
sw_interval_plot <-
     function(x,
              area_fill = "tomato3",
              line_color = "tomato3") {
          # input validation
          if (!is.data.frame(x) || !all(c("year", "n_sapwood", "p") %in% names(x))) {
               stop("Input must be a data.frame as returned by sw_interval(hdi = FALSE)")
          }
          # to avoid notes in CMD check
          year <- NULL


          # extract attributes
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

          # Merge: only preserve year from observed data (x)
          df <- merge(p_model, x, by = "n_sapwood", all = TRUE)
          colnames(df)[colnames(df) == "p.x"] <- "p_model"
          colnames(df)[colnames(df) == "p.y"] <- "p_obs"

          # Recover or recompute year column if necessary
          if (!"year" %in% colnames(df) || any(is.na(df$year))) {
               last_year <- max(x$year, na.rm = TRUE)
               df$year <- seq(last_year - nrow(df) + 1, last_year, by = 1)
          }

          max_p <- max(df$p_model, na.rm = TRUE)
          lower_hdi <- hdi_p[[1]]
          upper_hdi <- hdi_p[[2]]

          # Prepare subset of data for HDI fill
          df_hdi <- df[df$year >= lower_hdi & df$year <= upper_hdi, ]

          # filter NA values to avoid warnings
          df <- df[!is.na(df$p_model) & !is.na(df$year), ]

          p <- ggplot2::ggplot(data = df) +
               ggplot2::geom_line(
                    ggplot2::aes(x = year, y = p_model),
                    color = "grey40",
                    linewidth = 0.1
               ) +
               ggplot2::geom_area(
                    data = df_hdi,
                    ggplot2::aes(
                         x = year,
                         y = p_model
                    ),
                    fill = area_fill,
                    color = line_color,
                    alpha = 0.3,
                    linewidth = 1
               ) +
               ggplot2::annotate(
                    "segment",
                    x = lower_hdi, xend = upper_hdi,
                    y = 0, yend = 0,
                    linewidth = 0.8
               ) +
               ggplot2::annotate(
                    "text",
                    x = max(df$year, na.rm = TRUE), ,
                    y = max_p,
                    label = paste0(
                         "date last measured ring: ",
                         min(x$year, na.rm = TRUE),
                         "\nmeasured sapwood rings: ",
                         min(x$n_sapwood),
                         "\nsapwood data: ",
                         sw_data_p,
                         "\nmodel: ",
                         densfun_p,
                         "\nhdi: ",
                         lower_hdi, " - ", upper_hdi,
                         "\ncred. mass: ",
                         round(100 * cred_mass_p), "%"
                    ),
                    hjust = 1,
                    vjust = 1
               ) +
               ggplot2::theme_minimal() +
               ggplot2::scale_x_continuous(
                    limits = c(
                         floor(min(df$year, na.rm = TRUE) / 10) * 10,
                         ceiling(max(df$year, na.rm = TRUE) / 10) * 10
                    ),
                    breaks = seq(
                         floor(min(df$year, na.rm = TRUE) / 10) * 10,
                         ceiling(max(df$year, na.rm = TRUE) / 10) * 10,
                         10
                    )
               ) +
               ggplot2::ylab("p") +
               ggplot2::xlab("calendar year")

          if (exists("theme_fdr", mode = "function")) {
               p <- p + theme_fdr()
          }

          suppressWarnings(p)
     }
