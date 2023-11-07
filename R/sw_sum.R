#' Compute the summed probability of multiple felling date ranges
#'
#' @param x A `data.frame` containing columns c("series", "swr", "waneyedge" ,
#'   "last"), Column "sw_model" is optional. Could be the output of read_fh(x,
#'   header = TRUE)
#' @param series Name of the column in `x` containing the ID's of the series.
#' @param last Name of the column in `x`containing the calendar year assigned to
#'   the last measured ring (should be `numeric`).
#' @param n_sapwood Name of the column in `x` where the number of observed
#'   sapwood rings are listed (should be `numeric`).
#' @param waneyedge Name of the column in `x` indicating the presence
#'   (TRUE)/absence (FALSE) of waney edge (should be a `logical` vector).
#' @param credMass A `scalar [0, 1]` specifying the mass within the credible
#'   interval (default = .954).
#' @param sw_data The name of the sapwood data set to use for modelling. Should
#'   be one of [sw_data_overview()], or the path to a .csv file with columns
#'   ´n_sapwood´ and ´count´.
#' @param densfun Name of the density function fitted to the sapwood data set.
#'   Should be one of:
#'   * "lognormal" (the default value),
#'   * "normal",
#'   * "weibull",
#'   * "gammma".
#' @param plot A `logical`.
#'   * If `TRUE`, [sw_sum_plot()] is triggered and a ggplot-style graph is
#'   returned with the summed probability density (SPD).
#'   * If `FALSE`, a list with the numeric output of the modelling process is returned.
#' @param scale_p A `logical`. If `TRUE` the summed probability density is
#'   scaled to 1.
#' @param sep Should be "," (comma)  or ";" (semi-colon) and is used when a
#'   sapwood data set is provided from user-defined .csv-file.
#'
#' @description Computes the summed probability density (SPD) for a set of
#'   felling date ranges.
#'
#' @return Depends on the value of `plot.`
#' @export
#' @seealso [sw_sum_plot()]
#' @examples
#' fellingdateR:::dummy7
#'
#' sw_sum(fellingdateR:::dummy7, plot = TRUE)
#'
sw_sum <- function (x,
                    series = "series",
                    last = "last",
                    n_sapwood = "n_sapwood",
                    waneyedge = "waneyedge",
                    sw_data = "Hollstein_1980",
                    densfun = "lognormal",
                    credMass = 0.954,
                    plot = FALSE,
                    scale_p = FALSE,
                    sep = ";") {
        df <- as.data.frame(x)

        cambium <- df[[waneyedge]]
        if (!is.logical(cambium)) {
                which_wk <- which(grepl("wk", cambium, ignore.case = TRUE))
                converted_T <- cambium[which_wk]
                converted_F <- cambium[-which_wk]
                cambium <-
                        ifelse(grepl("wk", cambium, ignore.case = TRUE),
                               TRUE,
                               FALSE)
                warning(
                        paste0(
                                "Column 'waneyedge' in data.frame should be:\n",
                                "a logical vector (TRUE/FALSE), indicating the presence of waney edge.\n"
                        )
                )
                warning(
                        sprintf(
                                "'%s' --> Converted to TRUE based on presence of string 'wK'.\n",
                                converted_T
                        )
                )
                warning(
                        sprintf(
                                "'%s' --> Converted to FALSE based on absence of string 'wK'.\n",
                                converted_F
                        )
                )

        }

        swr <- df[[n_sapwood]]
        if (is.character(swr)) {
                stop("--> 'n_sapwood' must be a numeric vector")
        }

        which.nna <- which(is.na(swr) & !cambium)
        if (length(which.nna) > 0) {
                df <- df[-which.nna,]
                swr <- df[[n_sapwood]]
                cambium <- df[[waneyedge]]
                if (!is.logical(cambium)) {
                        cambium <-
                                ifelse(grepl("wk", cambium, ignore.case = TRUE),
                                       TRUE,
                                       FALSE)
                }
                warning(
                        paste0(
                                "--> ",
                                length(which.nna),
                                " series without sapwood rings or waney edge detected\n and removed from the data set"
                        )
                )
        }

        if (nrow(df) == 0) {
                stop("--> No series with sapwood or waney edge in dataset")
        }

        keycodes <- df[[series]]
        keycodes <- as.character(keycodes)
        if (any(is.na(keycodes))) {
                stop("--> Some 'series' have no id")
        }

        endDate <- df[[last]]
        if (!is.numeric(endDate)) {
                stop("--> 'last' must be a numeric vector")
        }

        # sw_model fixed for all series
        if (sw_data %in% sw_data_overview() ||
            file.exists(sw_data)) {
                sw_data <- rep(sw_data, nrow(df))
        }
        # sw_model might differ between series and is provided in a separate column
        else if (sw_model %in% colnames(df)) {
                sw_data <- df[[sw_data]]
                sw_OK <-
                        which(sw_data %in% sw_data_overview() |
                                      file.exists(sw_data))
                if (length(sw_OK) < length(sw_data)) {
                        stop(
                                sprintf(
                                        "'%s' is not a supported sapwood model of file doesn't exist\n",
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


        if (is.na(credMass) || credMass <= 0 || credMass >= 1)
                stop("--> credMass must be between 0 and 1")


        wk.true <- which(cambium)
        wk.true <- keycodes[wk.true]

        timeRange <- range(df[, last])
        timeAxis <- seq(timeRange[1], timeRange[2] + 100, by = 1)

        pdf_matrix <- matrix(nrow = length(timeAxis), ncol = 1)


        pdf_matrix[, 1] <- timeAxis
        colnames(pdf_matrix) <- "year"

        for (i in 1:length(keycodes)) {
                keycode_i <- keycodes[i]
                swr_i <- swr[i]
                yr <- endDate[i]
                cambium_i <- cambium[i]
                sw_data_i <- sw_data[i]

                if (cambium_i == TRUE) {
                        fellingDate <- df[i, last]
                        pdf <- matrix(NA, nrow = 1, ncol = 2)
                        colnames(pdf) <- c("year", keycode_i)
                        pdf[1, 1] <- fellingDate
                        pdf[1, 2] <- 1
                        pdf_matrix <-
                                merge(pdf_matrix,
                                      pdf,
                                      by = "year",
                                      all = TRUE)
                } else {
                        pdf <-
                                sw_interval(
                                        n_sapwood = swr_i,
                                        last = yr,
                                        sw_data = sw_data_i,
                                        densfun = densfun,
                                        sep = sep
                                )
                        pdf <- pdf[, -2] # remove column n_sapwood
                        colnames(pdf) <- c("year", keycode_i)
                        pdf_matrix <-
                                merge(pdf_matrix,
                                      pdf,
                                      by = "year",
                                      all = TRUE)
                }
        }

        # sum probabilities to SPD
        if (length(wk.true) == 0) {
                tmp <- pdf_matrix[, "year", drop = FALSE]
                tmp$SPD_wk <- 0

        } else {
                tmp <- pdf_matrix[, c("year", wk.true), drop = FALSE]
                if (dim(tmp)[2] > 2)
                        tmp$SPD_wk <-
                                rowSums(tmp[, -1], na.rm = TRUE)
                if (ncol(tmp) == 2)
                        tmp$SPD_wk <- tmp[, 2]
                if (ncol(tmp) == 1)
                        tmp$SPD_wk <- 0
                tmp <- tmp[, c("year", "SPD_wk")]

        }

        tmp2 <-
                pdf_matrix[, setdiff(names(pdf_matrix), wk.true), drop = FALSE]

        if (dim(tmp2)[2] > 2)
                pdf_matrix$SPD <- rowSums(tmp2[, -1], na.rm = TRUE)
        if (dim(tmp2)[2] == 2)
                pdf_matrix$SPD <- tmp2[, 2]
        if (dim(tmp2)[2] < 2)
                pdf_matrix$SPD <- 0

        pdf_matrix <-
                merge(pdf_matrix, tmp, by = "year", all = TRUE)

        # scale SPD to 1
        if (scale_p == TRUE) {
                pdf_matrix$SPD <- pdf_matrix$SPD / sum(pdf_matrix$SPD, na.rm = TRUE)

        }

        if (plot) {
                sw_sum_plot(pdf_matrix)
        }
        else {
                return(pdf_matrix)
        }
}
