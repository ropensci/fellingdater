#' Plot the output of [sw_combine()]
#'
#' @description This function plots the result of `sw_combine()` and the
#'   interval for, or the exact felling date of, a group of tree-ring series,
#'   with agreement indices for the global model.
#'
#' @param x Output of [sw_combine()].

#' @return A ggplot-style graph, with calendar years on the X-axis and the
#' probability (p) on the Y-axis. Dots represent exact felling dates.
#' @export
#' @examples
#' trs_example2
#' tmp <- sw_combine(trs_example2)
#' sw_combine_plot(tmp)
#'
sw_combine_plot <- function(x) {

  if (all(
    !(attributes(x)$names) %in% c(
      "raw_data",
      "sapwood_data",
      "sapwood_model",
      "cred_mass",
      "hdi_model",
      "hdi_combine",
      "individual_series",
      "A_c",
      "A_model",
      "model_summary"
    )
  )) {
    stop("Input differs from output sw_combine()")
  }

  # to avoid notes in CMD check
  year <-
    p <-
    lower <-
    upper <- comb <- last <- n_sapwood <- agr_index <- agreement <- NULL


  series <- colnames(x$raw_data)[2:(ncol(x$raw_data) - 1)]

  n <- length(series)
  cred_mass <- x$cred_mass
  sw_data <- x$sapwood_data
  densfun <- x$sapwood_model

  pdf <- x$raw_data
  pdf[, 2:ncol(x$raw_data)] <- apply(pdf[, 2:ncol(x$raw_data)],
    2,
    FUN = rescale
  )
  pdf <-
    as.data.frame(pdf) # added to avoid warning by dplyr::filter

  combo <- pdf[, c("year", "comb")]
  combo <-
    as.data.frame(combo) # added to avoid warning by dplyr::filter

  range <- range(pdf$year)
  if (length(range < 20)){range[2] <- range[2] + 20}

  # creates a vector with series that only have a terminus post quem
  tpq <- which(apply(
    x$raw_data,
    2,
    FUN = function(x) {
      sum(x, na.rm = TRUE)
    }
  ) == 0)
  tpq <- names(tpq)
  tpq <- setdiff(tpq, c("comb"))

  tpq_min <- x$hdi_model[[1]]

  hdi <- x$hdi_combine

  summary <- x$individual_series
  summary$agreement <-
    ifelse(summary$agr_index >= 60 | is.na(summary$agr_index),
      "good", "poor"
    )

  pdf <-
    pdf |>
    tidyr::pivot_longer(-year, names_to = "series", values_to = "p") |>
    dplyr::filter(series != "comb") |>
    dplyr::mutate(series = factor(series))


  p <- ggplot2::ggplot(pdf, ggplot2::aes(x = year, y = p)) +

    # plot the pdf for each series with swr or exact felling date
    {
      if (length(tpq) < nrow(summary)) {
        ggplot2::geom_area(
          fill = "grey90",
          alpha = 0.7,
          na.rm = TRUE
        )
      }
    } +
    {
      if (length(tpq) < nrow(summary)) {
        ggplot2::geom_line(
          color = "grey40",
          linewidth = 0.5,
          na.rm = TRUE
        )
      }
    } +

    # plot tpq as arrow pointing away from end date
    {
      if (length(tpq) > 0) {
        ggplot2::geom_point(
          data = pdf |>
            dplyr::filter(series %in% tpq) |>
            dplyr::filter(!is.na(p)) |>
            dplyr::group_by(series) |>
            dplyr::summarize(max = max(year)),
          ggplot2::aes(x = max + 1, y = .7),
          size = 2,
          fill = "darkblue",
          na.rm = TRUE
        )
      }
    } +
    {
      if (length(tpq) > 0) {
        ggplot2::geom_segment(
          data = pdf |>
            dplyr::filter(series %in% tpq) |>
            dplyr::filter(!is.na(p)) |>
            dplyr::group_by(series) |>
            dplyr::summarize(max = max(year)),
          ggplot2::aes(
            x = max + 1,
            xend = max + tpq_min + 1,
            y = .7,
            yend = .7,
            group = series
          ),
          arrow = ggplot2::arrow(length = ggplot2::unit(0.08, "npc")),
          na.rm = TRUE
        )
      }
    } +
    # # draw a line delimiting the hdi of the individual felling date estimate
    ggplot2::geom_segment(
      data = summary,
      ggplot2::aes(
        x = lower,
        xend = upper,
        y = -.05,
        yend = -.05
      ),
      colour = "tomato3",
      linewidth = .8,
      linetype = "dotted",
      na.rm = TRUE
    ) +

    # # add the combined felling date estimate as background
    {
      if (!is.na(hdi[[2]])) {
        ggplot2::geom_area(
          data = combo,
          ggplot2::aes(x = year, y = comb),
          fill = "grey50",
          alpha = 0.5,
          na.rm = TRUE
        )
      }
    } +
    {
      if (!is.na(hdi[[2]])) {
        ggplot2::geom_line(
          data = combo,
          ggplot2::aes(x = year, y = comb),
          color = "grey30",
          na.rm = TRUE
        )
      }
    } +

    # draw a line delimiting the hdi of the combined felling date estimate
    {
      if (!is.na(hdi[[2]])) {
        ggplot2::geom_segment(
          ggplot2::aes(
            x = hdi[[1]],
            xend = hdi[[2]],
            y = -0.1,
            yend = -0.1
          ),
          color = "black",
          linewidth = 1,
          na.rm = TRUE
        )
      }
    } +

    # add summary text
    ggplot2::geom_text(
      data = summary,
      ggplot2::aes(
        x = ceiling((range[2]) / 10) * 10,
        y = 0.6,
        label = paste0(
          series,
          " (last: ",
          last,
          ", swr: ",
          ifelse(is.na(n_sapwood), "- ", as.character(n_sapwood)),
          ")",
          "\n "
          # ifelse(is.na(agr_index),
          #   "",
          #   paste0("A_i = ", round(
          #     as.numeric(agr_index), 1
          #   ), "%")
          # )
        ),
        hjust = 1
      ),
      na.rm = TRUE
    ) +
    ggplot2::geom_text(
      data = summary,
      ggplot2::aes(
        x = ceiling((range[2]) / 10) * 10,
        y = 0.40,
        label = ifelse(is.na(agr_index),
                             "",
                             paste0("A[i]", "==", round(
                               as.numeric(agr_index), 1
                             ),"*\'%'")),
        color = ifelse(agr_index < 60, "poor", "good")
      ),
      hjust = 1, parse = TRUE
    ) +
    ggplot2::scale_color_manual(values = c("good" = "black" ,"poor" = "tomato3")) +
    ggplot2::facet_grid(dplyr::vars(as.factor(series)), ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(
      limits = c(
        floor(range[1] / 10) * 10,
        ceiling((range[2]) / 10) * 10
      ),
      breaks = seq(
        floor(range[1] / 10) * 10,
        ceiling((range[2]) / 10) * 10,
        20
      )
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 10),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "none",
      strip.background = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank()
    ) +
    ggplot2::xlab("calendar year") +

    ggplot2::labs(
      title = x$model_summary,
      subtitle = paste0(
        if (!is.na(x$A_model)) {
          paste("A_model = ", x$A_model, "% (Ac = 60%)")
        },
        if (!is.na(x$A_model) &
          x$A_model < 60) {
          " model fails (A_model < Ac)"
        },
        if (grepl(pattern = "multiple", x$model_summary)) {
          "(unable to combine)"
        }
      ),
      caption = paste0(100 * cred_mass, "% credible interval (hdi)")
    ) +
    {
      if ((x$A_model < 60 & !is.na(x$A_model)) |
        grepl(pattern = "multiple", x$model_summary)) {
        ggplot2::theme(plot.subtitle = ggplot2::element_text(color = "tomato3"))
      }
    }

  suppressWarnings(print(p))
}
