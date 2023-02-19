#' sw_combine_plot: plot the output of `sw_combine()`
#'
#' @description This function plots the result of `sw_combine()` and the
#'   interval for, or the exact felling date of a group of tree-ring series,
#'   with agreement indices for the global model.
#'
#' @param x Output of `sw_combine()`.

#' @return A ggplot style graph.
#' @export
#'
#'
sw_combine_plot <- function(x) {

     ########################################################################
     # function to rescale probabilities between [0, 1].
     rescale <- function(x, floor = 0, ceiling = 1){
          if (max(x, na.rm = T) == 0) {x}
          else{
               (x - min(x, na.rm = T))*(ceiling - floor)/(max(x, na.rm = T) - min(x, na.rm = T))
          }
     }
     ########################################################################

     if (all(!(attributes(x)$names) %in% c("rawData", "sapwood_data",
                                           "sapwood_model", "credMass",
                                           "hdi_model", "hdi_combine",
                                           "individual_series",
                                           "A_c", "A_comb",
                                           "model_summary")))
          stop("Input differs from output sw_combine()")

     # to avoid notes in CMD check
     year <- p <- lower <- upper <- COMB <- last <- n_sapwood <- A_i <- agreement <- NULL


     series <- colnames(x$rawData)[2:(ncol(x$rawData)-1)]

     n <- length(series)
     credMass <- x$credMass
     sw_data <- x$sapwood_data
     densfun <- x$sapwood_model

     pdf <- x$rawData
     pdf[, 2:ncol(x$rawData)] <- apply(pdf[, 2:ncol(x$rawData)],
                                       2,
                                       FUN = rescale)

     combo <- pdf[, c("year", "COMB")]

     range <- range(pdf$year)

     # creates a vector with series that only have a terminus post quem
     tpq <- which(apply(x$rawData,
                        2,
                        FUN = function(x) sum(x, na.rm = TRUE)) == 0)
     tpq <- names(tpq)
     tpq <- setdiff(tpq, c("COMB"))

     tpq_min <- x$hdi_model[[1]]

     hdi <- x$hdi_combine

     summary <- x$individual_series
     summary$agreement <- ifelse(summary$A_i >= 60 | is.na(summary$A_i),
                                 "good", "poor")

     pdf <-
          pdf |>
          tidyr::pivot_longer(-year, names_to = "series", values_to = "p") |>
          dplyr::filter(series != "COMB") |>
          dplyr::mutate(series = factor(series))


     ggplot2::ggplot(pdf, ggplot2::aes(x= year, y = p)) +

          # plot the pdf for each series with swr or exact felling date
          { if (length(tpq) < nrow(summary))
               ggplot2::geom_area(fill = "grey90", alpha = 0.7) } +
          { if (length(tpq) < nrow(summary))
               ggplot2::geom_line(color = "grey40", size = 0.5) } +

          # plot tpq as arrow pointing away from end date
          { if (length(tpq) > 0)
               ggplot2::geom_point(data = pdf |>
                                        dplyr::filter(series %in% tpq) |>
                                        dplyr::filter(!is.na(p)) |>
                                        dplyr::group_by(series) |>
                                        dplyr::summarize(max = max(year)),
                                   ggplot2::aes(x = max + 1, y = .7),
                                   size = 2, fill = "darkblue")
          } +
          { if (length(tpq) > 0)
               ggplot2::geom_segment(data = pdf |>
                                          dplyr::filter(series %in% tpq) |>
                                          dplyr::filter(!is.na(p)) |>
                                          dplyr::group_by(series) |>
                                          dplyr::summarize(max = max(year)),
                                     ggplot2::aes(
                                          x = max + 1, xend = max + tpq_min + 1,
                                          y = .7, yend = .7, group = series),
                                     arrow = ggplot2::arrow(length = ggplot2::unit(0.08, "npc")))
          } +
          # # draw a line delimiting the hdi of the individual felling date estimate (credMass)
          ggplot2::geom_segment(data = summary,
                                ggplot2::aes(
                                     x = lower, xend = upper,
                                     y = -.05, yend = -.05),
                                colour = "tomato3", size = .8,linetype = "dotted") +

          # # add the combined felling date estimate as background
          { if (!is.na(hdi[[2]]))
               ggplot2::geom_area(data = combo,
                                  ggplot2::aes(x = year, y = COMB),
                                  fill = "grey50", alpha = 0.5) } +
          { if (!is.na(hdi[[2]]))
               ggplot2::geom_line(data = combo,
                                  ggplot2::aes(x = year, y = COMB),
                                  color = "grey30") } +

          # draw a line delimiting the hdi of the combined felling date estimate (credMass)
          { if (!is.na(hdi[[2]]))
               ggplot2::geom_segment(
                    ggplot2::aes(x = hdi[[1]], xend = hdi[[2]],
                                 y = -0.1, yend = -0.1),
                    color = "black", size = 1)
          } +

          # add summary text
          ggplot2::geom_text(data = summary, ggplot2::aes(
               x = plyr::round_any(range[2] + 40, 10, ceiling),
               y = 0.6,
               label = paste0(
                    series,
                    " (last: ",
                    last,
                    ", swr: ",
                    ifelse(
                         is.na(n_sapwood), "- ", as.character(n_sapwood)),
                    ")", "\n ",
                    ifelse(is.na(A_i),
                           "",
                           paste0("A_i = ", round(as.numeric(A_i), 1), "%"))),
               hjust = 1)) +

          { if (nrow(summary |> dplyr::filter(agreement == "poor")) != 0)
               ggplot2::geom_text(data = summary |>  dplyr::filter(agreement == "poor"),
                                  ggplot2::aes(x = plyr::round_any(range[2] + 40, 10, ceiling),
                                               y = 0.15,
                                               label = "poor agreement",
                                               color = "tomato3", hjust = 1))
          } +

          ggplot2::facet_grid(dplyr::vars(as.factor(series)), ) +
          ggplot2::theme_minimal() +

          ggplot2::scale_x_continuous(
               limits = c(plyr::round_any(range[1], 10, floor),
                          plyr::round_any(range[2] + 40, 10, ceiling)),
               breaks = seq(plyr::round_any(range[1], 10, floor),
                            plyr::round_any(range[2] + 40, 10, ceiling),
                            10)) +

          ggplot2::theme(axis.text=ggplot2::element_text(size=12),
                         axis.title.x=ggplot2::element_blank(),
                         axis.title.y=ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_blank(),
                         panel.grid.minor.y=ggplot2::element_blank(),
                         panel.grid.major.y=ggplot2::element_blank(),
                         legend.position = "none",
                         strip.background = ggplot2::element_blank(),
                         strip.text.y = ggplot2::element_blank()
          ) +

          ggplot2::labs(title = x$model_summary,
                        subtitle = paste0(if(!is.na(x$A_comb)) paste("A_model = ", x$A_comb, "% (Ac = 60%)"),
                                          if(!is.na(x$A_comb) & x$A_comb <60) " model fails (A_model < Ac)"),
                        caption = paste0(100*credMass, "% credible interval (hdi)")) +
          { if(x$A_comb < 60 & !is.na(x$A_comb)) ggplot2::theme(plot.subtitle = ggplot2::element_text(color = "tomato3")) }

}
