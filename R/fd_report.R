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
#'   for modelling. Should be one of [sw_data_overview()], or the path to a .csv
#'   file with columns ´n_sapwood´ and ´count´. This variable will be used for
#'   all individual series in `x`, or
#'
#'    * the name of the column in `x`that lists for each series one of the
#'   sapwood data sets given by [sw_data_overview()], or the path to a .csv file
#'   with columns ´n_sapwood´ and ´count´.
#' @param credMass A `scalar [0, 1]` specifying the mass within the credible
#'   interval (default = .954).
#' @param densfun Name of the density function to fit to the sapwood data set.
#'   Should be one of:
#'   * "lognormal" (the default value),
#'   * "normal",
#'   * "weibull",
#'   * "gammma".
#' @param sep Should be "," (comma)  or ";" (semi-colon) and is used when a
#'   sapwood data set is provided from user-defined .csv-file.
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
#' fellingdateR:::dummy2
#' sw_models_for_indiv_series <- c("Sohar_2012_ELL_c",
#'                                 "Wazny_1990",
#'                                 "Hollstein_1980",
#'                                 "vanDaalen_Norway",
#'                                 "vanDaalen_Norway")
#'
#' dummy2_edit <- cbind(fellingdateR:::dummy2, sw_models_for_indiv_series)
#'
#' fd_report(dummy2_edit,
#'           sw_data = "sw_models_for_indiv_series"
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
                      credMass = 0.954,
                      densfun = "lognormal",
                      sep = ";") {
        df <- as.data.frame(x)
        # Check columns exist
        if (!series %in% names(df)) {
                stop("--> 'series' does not exist")
        }
        if (!last %in% names(df)) {
                stop("--> 'last' does not exist")
        }
        if (!n_sapwood %in% names(df)) {
                stop("--> 'n_sapwood' does not exist")
        }
        if (!waneyedge %in% names(df)) {
                stop("--> 'waneyedge' does not exist")
        }
        series <- df[[series]] # check for NA's
        if (any(is.na(series))) {
                stop("--> some 'series' have no id")
        }
        n_sapwood <- df[[n_sapwood]] # check is.numeric
        if (is.character(n_sapwood)) {
                # was !is.numeric !!!
                stop("--> 'n_sapwood' must be a numeric vector")
        }
        last <- df[[last]] # check is.numeric
        if (!is.numeric(last)) {
                stop("--> 'last' must be a numeric vector")
        }
        waneyedge <- df[[waneyedge]] # check is.logical
        if (!is.logical(waneyedge)) {
                stop("--> 'waneyedge' should be a logical vector (TRUE/FALSE),
indicating the presence of waney edge.\n")
        }
        if (is.na(credMass) || credMass <= 0 || credMass >= 1)
                stop("--> credMass must be between 0 and 1")


        # sw_data fixed for all series
        if (sw_data %in% sw_data_overview() || file.exists(sw_data)) {
                sw_data <- rep(sw_data, nrow(df))
        }
        # sw_data might differ between series and is provided in a separate column
        else if (sw_data %in% colnames(df)) {
                sw_data <- df[[sw_data]]
                sw_OK <-
                        which(sw_data %in% sw_data_overview() | file.exists(sw_data))
                if (length(sw_OK) < length(sw_data)) {
                        stop(
                                sprintf(
                                        "'%s' is not a supported sapwood model, or file does not exist\n",
                                        sw_data[-sw_OK]
                                )
                        )
                }
        } else {
                stop(
                        sprintf(
                                "--> sw_data should be one of `sw_data_overview()`
or the path to a .csv file with columns `n_sapwood` and `count`,\n
not '%s'.",
sw_data
                        )
                )
        }

        interval_matrix <- matrix(nrow = nrow(df),
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
                                credMass = credMass,
                                sw_data = sw_data_i,
                                densfun = densfun,
                                sep = sep
                        )
                        lower_i <- interval_i[[1]]
                        upper_i <- interval_i[[2]]
                } else if (is.na(n_sapwood_i) && !is.na(last_i)) {
                        interval_i <- sw_interval(
                                n_sapwood = 0,
                                last = last_i,
                                hdi = TRUE,
                                credMass = credMass,
                                sw_data = sw_data_i,
                                densfun = densfun,
                                sep = sep
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
                lapply(c(2, 3, 5, 6), function(x)
                        as.numeric(interval_matrix[, x]))
        interval_matrix[, 4] <- as.logical(interval_matrix[, 4])

        attr(interval_matrix, "credMass") <- credMass
        attr(interval_matrix, "model") <- densfun

        return(interval_matrix)

}
