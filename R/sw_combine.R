#' sw_combine: compute a felling date range by combining multiple sapwood estimates
#'
#' @param x Name of a `data.frame` with at least four columns, providing
#'   information on the id's of the tree-ring series, the number of sapwood
#'   rings observed, the presence of waney edge and the date assigned to the
#'   last measured ring. A column describing the sapwood data set to be used
#'   for modelling and the computation of the hdi can be provided as well.
#' @param series Name of the column in `x` where id's of the tree-ring series
#'   are listed as `character` values.
#' @param last Name of the column in `x` which lists the calendar year assigned
#'   to the last measured ring. Should be `numeric`.
#' @param n_sapwood Name of the column in `x` where the number of observed
#'   sapwood rings are listed. This variable should be `numeric`.
#' @param waneyedge Name of the column in `x` indicating the presence
#'  (`TRUE`)/absence (`FALSE`) of waney edge. Should be a `logical` vector.
#' @param sw_data The name of the sapwood data set to use for modelling.
#'  Should be one of [sw_data_overview()], or the path to a .csv file with
#'  columns ´n_sapwood´ and ´count´.
#' @param credMass A `scalar [0, 1]` specifying the mass within the credible
#'   interval (default = .954).
#' @param hdi A `logical.` If `TRUE`: the lower and upper limit of the
#'   highest density interval (credible interval) is given for the felling date.
#'   When `FALSE`: a matrix is returned with scaled p values for calendar years
#'   covering the combined estimate of the felling date range.
#' @param plot A `logical`. If `TRUE` a plot is returned of the individual and
#'   combined estimate of the felling date.
#'   If `FALSE` a list with numeric output of the modelling process is returned.
#' @param densfun Name of the density function fitted to the sapwood data set.
#'   Should be one of:
#'   * "lognormal" (the default value),
#'   * "normal",
#'   * "weibull",
#'   * "gammma".
#' @param sep Should be "," (comma)  or ";" (semi-colon) and is used when a
#'   sapwood data set is provided from user-defined .csv-file.
#'
#' @description Combine multiple estimates of the felling date for a single event.
#' @return Depends on the value of plot. If TRUE a ggplot style
#'   When `FALSE` a `data.frame` with the combined probability and modelling parameters
#' @export
#'
#' @examples
#' # a data set in which all series have preserved sapwood
#'
#' dummy1 <- data.frame(
#' series = c("trs_06", "trs_07", "trs_08", "trs_09", "trs_10"),
#' last = c(1000, 1009, 1007, 1005, 1010),
#' n_sapwood = c(5, 10, 15, 16, 8),
#' waneyedge = c(FALSE, FALSE, FALSE, FALSE, FALSE)
#' )
#' sw_combine(dummy1, plot = TRUE)
#'
#' # a data set in which one series has an exact felling date (= waney edge preserved)
#'
#' dummy2 <- data.frame(
#' series = c("trs_11", "trs_12", "trs_13", "trs_14", "trs_15"),
#' last = c(1000, 1005, 1008, 1000, 1010),
#' n_sapwood = c(5, 10, NA, 1, 3),
#' waneyedge = c(FALSE, FALSE, FALSE, FALSE, TRUE)
#' )
#' sw_combine(dummy2, plot = TRUE)
#'
#' sw_combine(fellingdateR:::dummy4, plot= FALSE)
#'
sw_combine <- function(x,
                       series = "series",
                       last = "last",
                       n_sapwood = "n_sapwood",
                       waneyedge = "waneyedge",
                       sw_data = "Hollstein_1980",
                       densfun = "lognormal",
                       sep = ";",
                       credMass = 0.954,
                       hdi = TRUE,
                       plot = FALSE){

     x <- as.data.frame(x)
     n <- nrow(x)
     endDate <- x[, last]

     if (any(is.na(as.numeric(endDate))) | is.factor(endDate) | is.logical(endDate)) {
          stop(
               "--> input does not provide end dates in the second column
(some are empty or not numeric)")
     }
     swr <- x[, n_sapwood]
     if (is.character(swr)) stop(
          "--> 'n_sapwood' must be a numeric vector")

     cambium <- x[, waneyedge]
     if (!is.logical(cambium)) {
          warning(
               "Column 'waneyedge' in data.frame should be logical (TRUE/FALSE), indicating
the presence of waney edge.\n",
" --> Converted to TRUE/FALSE based on presence of string 'wK'.")
          cambium <- ifelse(grepl("wk", cambium, ignore.case = TRUE),
                            TRUE,
                            FALSE)
     }

     timeRange <- range(endDate)
     timeAxis <- seq(timeRange[1]-3, timeRange[2] + 100, by = 1)

     pdf_matrix <- matrix(nrow = length(timeAxis), ncol = 1)
     colnames(pdf_matrix) <- "year"
     pdf_matrix[, 1] <- timeAxis
     keycodes <- x[, series]
     keycodes <- as.character(keycodes)

     hdi_model <- sw_interval(n_sapwood = 0,
                              last = 0,
                              hdi = TRUE,
                              credMass = credMass,
                              sw_data = sw_data,
                              densfun = densfun,
                              sep = sep)
     rownames(hdi_model) <- NULL
     hdi_min <- hdi_model[[1]]
     hdi_max <- hdi_model[[2]]

     # no series with sapwood or waney edge --> compute terminus post quem
     if (all(cambium == FALSE) & all(is.na(swr))) {
          most_recent <- max(endDate)
          tpq <- most_recent + hdi_min

          message <- paste0("earliest possible felling date: ", tpq)

          for (i in 1:length(keycodes)){
               keycode_i <- keycodes[i]
               pdf <- matrix(NA, nrow = length(timeAxis), ncol = 2)
               pdf[, 1] <- timeAxis
               colnames(pdf) <- c("year", keycode_i)
               yr <- endDate[i]
               pdf[pdf[, "year"] < yr, keycode_i] <- 0
               pdf[pdf[, "year"] >= yr, keycode_i] <- NA
               pdf_matrix <- merge(pdf_matrix, pdf, by = "year", all = TRUE)
          }
          # result of prod(c(NA,NA), na.rm TRUE) == 1
          # first remove rows with all NA (except "year")
          pdf_matrix <- pdf_matrix[rowSums(is.na(pdf_matrix)) != ncol(pdf_matrix)-1, ]

          # combine probabilities into one probability density function
          if (n == 1){

               pdf_matrix$COMB <- pdf_matrix[, 2]

          } else {

               pdf_matrix$COMB <- apply(pdf_matrix[, -1], 1,
                                        FUN=function(x) prod(x, na.rm = TRUE))
          }

          hdi <- c("lower" = tpq, "upper" = NA_integer_)
          summary <- fd_report(x,
                               series = series,
                               last = last,
                               n_sapwood = n_sapwood,
                               waneyedge = waneyedge,
                               sw_data = sw_data,
                               credMass = 0.954,
                               densfun = densfun,
                               sep = ";")[, 1:6]
          summary$A_i <- NA
          rownames(summary) <- NULL
          A_c <- NA
          A_comb <- NA

     } else { # some series with sapwood or waney edge

          pdf_matrix[, 1] <- timeAxis
          colnames(pdf_matrix) <- "year"

          for (i in 1:length(keycodes)){
               keycode_i <- keycodes[i]
               swr_i <- swr[i]
               yr <- endDate[i]
               cambium_i <- cambium[i]

               if (cambium_i == TRUE){ # exact felling date
                    pdf <- matrix(NA, nrow = length(timeAxis), ncol = 2)
                    pdf[, 1] <- timeAxis
                    colnames(pdf) <- c("year", keycode_i)
                    pdf[pdf[, "year"] == yr, keycode_i] <- 1
                    pdf[pdf[, "year"] != yr, keycode_i] <- 0
                    pdf_matrix <- merge(pdf_matrix, pdf, by = "year", all = TRUE)

               } else if (is.na(swr_i)) { # terminus post quem date
                    pdf <- matrix(NA, nrow = length(timeAxis), ncol = 2)
                    pdf[, 1] <- timeAxis
                    colnames(pdf) <- c("year", keycode_i)
                    pdf[pdf[, "year"] < yr, keycode_i] <- 0
                    pdf[pdf[, "year"] >= yr, keycode_i] <- NA
                    pdf_matrix <- merge(pdf_matrix, pdf, by = "year", all = TRUE)

               } else {
                    #  apply sw_interval to each individual series
                    pdf <- sw_interval(n_sapwood = swr_i,
                                       last = yr,
                                       hdi = FALSE,
                                       credMass = credMass,
                                       sw_data = sw_data,
                                       densfun = densfun,
                                       sep = sep)
                    # remove column "n_sapwood" from output sw_interval
                    pdf <- pdf[c(1, 3)]
                    colnames(pdf) <- c("year", keycode_i)
                    pdf_matrix <- merge(pdf_matrix, pdf, by = "year", all = TRUE)
                    # fill matrix with 0's when NA
                    pdf_matrix[is.na(pdf_matrix[, keycode_i]), keycode_i] <- 0
               }
          }

          # result of prod(c(NA,NA), na.rm TRUE) == 1
          # first remove rows with all NA (except "year")
          pdf_matrix <- pdf_matrix[rowSums(is.na(pdf_matrix)) != ncol(pdf_matrix)-1, ]

          # combine probabilities into one probability density function
          if (n == 1){

               pdf_matrix$COMB <- pdf_matrix[, 2]

          } else {
               pdf_matrix$COMB <- apply(pdf_matrix[, -1], 1,
                                        FUN=function(x) prod(x, na.rm = TRUE))

               if (any(pdf_matrix[, 2:length(keycodes) +1] == 1, na.rm = TRUE)) {
                    # when multiple exact felling dates are listed that do no correspond
                    # --> COMB = 0 and after scaling NaN (division by 0)
                    # check rowwise if there is any p-value == 1,
                    # and replace COMB at that position with 1
                    pdf_matrix[apply(pdf_matrix[,2:length(keycodes)+1] == 1,
                                     1,
                                     FUN=function(x) any(x, na.rm = TRUE)), "COMB"] <- 1

               } else if (sum(pdf_matrix$COMB, na.rm = TRUE) > 0) {
                    # avoid division by 0
                    pdf_matrix$COMB <- pdf_matrix$COMB/sum(pdf_matrix$COMB, na.rm = TRUE)

               }
          }

          if (hdi == FALSE){

               return(pdf_matrix)

          } else if (hdi == TRUE & sum(pdf_matrix$COMB, na.rm = TRUE) == 0) {

               message <- paste0("unable to combine these ", n, " series")

               hdi <- c("lower" = NA, "upper" = NA)

               summary <- fd_report(x,
                                    series = series,
                                    last = last,
                                    n_sapwood = n_sapwood,
                                    waneyedge = waneyedge,
                                    sw_data = sw_data,
                                    credMass = 0.954,
                                    densfun = densfun,
                                    sep = ";")[, 1:6]
               summary$A_i <- NA
               rownames(summary) <- NULL

               A_c <- NA
               A_comb <- NA

          } else if (hdi == TRUE & (sum(pdf_matrix[,"COMB"], na.rm =TRUE) >= 2)) {
               # case with multiple and not corresponding felling dates

               fds <- pdf_matrix[pdf_matrix[,  "COMB"] == 1, "year"]
               fds <- paste0(fds, collapse = ", ")

               message <- paste0("multiple felling dates: ", fds)

               hdi <- c("lower" = NA, "upper" = NA)

               summary <- fd_report(x,
                                    series = series,
                                    last = last,
                                    n_sapwood = n_sapwood,
                                    waneyedge = waneyedge,
                                    sw_data = sw_data,
                                    credMass = 0.954,
                                    densfun = densfun,
                                    sep = ";")[, 1: 6]
               summary$A_i <- NA
               rownames(summary) <- NULL

               A_c <- NA
               A_comb <- NA


          } else {

               hdi <- hdi(pdf_matrix, a = "year", b = "COMB",
                             credMass = credMass)
               # hdi <- hdi(pdf_matrix[, c("year", "COMB")],
               #            credMass = credMass)

               if (hdi[1] == hdi[2]){
                    message <- paste("exact felling date: ", hdi[2])

               } else {
                    message <- paste("felling date range: ", hdi[1], " - ", hdi[2])

               }

               A <- 1
               A_i <- matrix(nrow = length(keycodes), ncol = 1)
               dimnames(A_i) <- list(keycodes, "A_i")

               for (i in 1:length(keycodes)){

                    if (is.na(swr[i]) & cambium[i] == FALSE){
                         A_i[i, 1] <- NA

                    } else {

                         temp  <- apply(pdf_matrix[, c(1+i, length(keycodes)+2)],
                                        1,
                                        FUN = function(x) prod(x, na.rm = TRUE))
                         temp <- sum(temp, na.rm = TRUE)
                         temp2 <- apply(pdf_matrix[, c(1+i, 1+i)],
                                        1,
                                        FUN = function(x) prod(x, na.rm = TRUE))
                         temp2 <- sum(temp2, na.rm = TRUE)
                         A_i[i, 1] <- round(temp/temp2*100, 1)
                         # A is the product of all individual A_i values
                         A <-  A_i[i, 1]/100 * A
                    }
               }

               # summary_old <- cbind(keycodes, endDate, swr, cambium, round(A_i, 1))
               summary <- fd_report(x,
                                    series = series,
                                    last = last,
                                    n_sapwood = n_sapwood,
                                    waneyedge = waneyedge,
                                    sw_data = sw_data,
                                    credMass = 0.954,
                                    densfun = densfun,
                                    sep = ";")[, 1: 6]
               summary$A_i <- round(A_i, 1)
               rownames(summary) <- NULL

               A_comb <- round(100 * A^(1/sqrt(length(keycodes))), 1)

               A_c <- 60

          }
     }
     names(A_comb) <- "Overall agreement index (%)"
     names(A_c) <- "Critical threshold (%)"


     model_summary <- list(
          rawData = pdf_matrix,
          sapwood_data = sw_data,
          sapwood_model = densfun,
          credMass = credMass,
          hdi_model = hdi_model,
          hdi_combine = hdi,
          individual_series = summary,
          A_comb = A_comb,
          A_c = A_c,
          model_summary = message)

     if (plot) {
          p <- sw_combine_plot(model_summary)
          suppressWarnings(print(p))

     } else {
          return(model_summary)

     }
}
