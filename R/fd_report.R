#' Report felling dates of individual tree-ring series
#'
#' @description This function reports the felling date estimate of individual
#'   tree-ring series, based on the presence/absence of sapwood and/or waney
#'   edge. There are three possible modes of reporting:
#'
#'   -  a _terminus post quem_ or _earliest possible felling date_: when only
#'   heartwood rings have been observed and measured
#'    - a felling date range or interval: when sapwood rings have been recorded, but
#'   no bark or waney edge is present.
#'    - an exact felling date: when bark or waney edge is present on the
#'   measured sample.
#'
#' @param x Name of a `data.frame` with at least four columms, providing
#'   information on
#'
#'   * the id's of the tree-ring series
#'   * the number of sapwood rings observed
#'   * the presence of waney edge
#'   * the date assigned to the last measured ring.
#'
#'   A column describing the sapwood data set to be used for modelling and the
#'   computation of the hdi can be provided as well.
#' @param series Name of the column in `x` where id's of the tree-ring series
#'   are listed as `character` values.
#' @param n_sapwood Name of the column in `x` where the number of observed
#'   sapwood rings are listed (should be `numeric` vector).
#' @param waneyedge Name of the column in `x` indicating the presence
#'   (`TRUE`)/absence (`FALSE`) of waney edge (should be a `logical` vector).
#' @param last Name of the column in `x` which lists the calendar year assigned
#'   to the last measured ring (should be a `numeric` vector).
#' @param sw_data There are two options:
#'    * A `character` string providing the name of the sapwood data set to use
#'    for modelling. It should be one of the data sets listed in
#'    [sw_data_overview()], or the name of a `data.frame` with sapwood data
#'    in columns `n_sapwood` and `count`, or
#'
#'    * the name of the column in `x`that lists for each series one of the
#'   sapwood data sets given by [sw_data_overview()].
#' @param cred_mass A `scalar [0, 1]` specifying the mass within the credible
#'   interval (default = .954).
#' @param densfun Name of the density function fitted to the sapwood data set.
#'   Should be one of:
#'   * _lognormal_ (the default value),
#'   * _normal_,
#'   * _weibull_,
#'   * _gammma_.
#'
#' @description Reports the lower and upper boundaries of a felling date range
#'   for individual tree-ring series.
#'
#' @return A data.frame reporting the estimate of the felling date for each
#'   tree-ring series?
#'
#' @examples
#' tmp <- data.frame(id = c("aaa", "bbb", "ccc"),
#'                   swr = c(10, 11, 12),
#'                   waneyedge = c(FALSE, FALSE,TRUE),
#'                   end = c(10, 0, -10))
#' fd_report(tmp,
#'           series = "id",
#'           n_sapwood = "swr",
#'           last = "end",
#'           sw_data = "Wazny_1990")
#'
#' # Example with different sw_model for individual series
#'
#' sw_models_for_indiv_series <- c("Sohar_2012_ELL_c",
#'                                 "Wazny_1990",
#'                                 "Hollstein_1980",
#'                                 "vanDaalen_Norway",
#'                                 "vanDaalen_Norway")
#'
#' trs_example2_edit <- cbind(trs_example2, "sw_models" = sw_models_for_indiv_series)
#'
#' fd_report(trs_example2_edit,
#'           sw_data = "sw_models"
#'           )
#'
#' @export
#' @seealso [sw_interval()], [sw_data_overview()], [sw_interval_plot()]

fd_report <- function(x,
                      series = "series",
                      last = "last",
                      n_sapwood = "n_sapwood",
                      waneyedge = "waneyedge",
                      sw_data = "Hollstein_1980",
                      cred_mass = 0.954,
                      densfun = "lognormal"
                      ) {

        check_input(x,
                    series = series,
                    last = last,
                    n_sapwood = n_sapwood,
                    waneyedge = waneyedge,
                    sw_data = sw_data,
                    cred_mass = cred_mass,
                    densfun = densfun)

        series <- x[, series]
        n_sapwood <- x[, n_sapwood]
        last <- x[, last]
        waneyedge <- x[, waneyedge]

        if (sw_data %in% sw_data_overview()) {
                sw_data <- rep(sw_data, nrow(x))
        } else if (exists(sw_data) & is.data.frame(sw_data)){
                sw_data <- rep(sw_data, nrow(x))
        } else if (sw_data %in% colnames(x)) {
                sw_data <- x[, sw_data]
        }

        interval_matrix <- matrix(nrow = nrow(x),
                                  ncol = 8)

        for (i in 1:length(series)) {
                series_i <- series[i]
                n_sapwood_i <- n_sapwood[i]
                last_i <- last[i]
                waneyedge_i <- waneyedge[i]
                sw_data_i <- sw_data[i]

                if (waneyedge_i) {
                        lower_i <- NA
                        upper_i <- last_i
                } else if (!is.na(n_sapwood_i) && !is.na(last_i)) {
                        interval_i <- sw_interval(
                                n_sapwood = n_sapwood_i,
                                last = last_i,
                                hdi = TRUE,
                                cred_mass = cred_mass,
                                sw_data = sw_data_i,
                                densfun = densfun
                        )
                        lower_i <- interval_i[[1]]
                        upper_i <- interval_i[[2]]
                } else if (is.na(n_sapwood_i) && !is.na(last_i)) {
                        interval_i <- sw_interval(
                                n_sapwood = 0,
                                last = last_i,
                                hdi = TRUE,
                                cred_mass = cred_mass,
                                sw_data = sw_data_i,
                                densfun = densfun
                        )
                        lower_i <- interval_i[[1]]
                        upper_i <- NA
                } else if (is.na(last_i)) {
                        lower_i <- NA
                        upper_i <- NA
                }

                if (!is.na(lower_i) && !is.na(upper_i)) {
                        verbal_i <- paste0("between ", lower_i, " and ", upper_i)
                } else if (!is.na(lower_i) && is.na(upper_i)) {
                        verbal_i <- paste0("after ", lower_i)
                } else if (is.na(lower_i) && !is.na(upper_i)) {
                        verbal_i <- paste0("in ", upper_i)
                } else if (is.na(lower_i) && is.na(upper_i)) {
                        verbal_i <- "undated"
                }

                interval_matrix[i, 1] <- series_i
                interval_matrix[i, 2] <- last_i
                interval_matrix[i, 3] <- n_sapwood_i
                interval_matrix[i, 4] <- waneyedge_i
                interval_matrix[i, 5] <- lower_i
                interval_matrix[i, 6] <- upper_i
                interval_matrix[i, 7] <- verbal_i
                interval_matrix[i, 8] <- sw_data_i

                colnames(interval_matrix) <- c(
                        "series",
                        "last",
                        "n_sapwood",
                        "waneyedge",
                        "lower",
                        "upper",
                        "felling_date",
                        "sapwood_model"
                )

        }

        interval_matrix <- as.data.frame(interval_matrix)
        interval_matrix[, c(2, 3, 5, 6)] <-
                lapply(c(2, 3, 5, 6), function(z)
                        as.numeric(interval_matrix[, z]))
        interval_matrix[, 4] <- as.logical(interval_matrix[, 4])

        attr(interval_matrix, "cred_mass") <- cred_mass
        attr(interval_matrix, "model") <- densfun

        return(interval_matrix)

}
