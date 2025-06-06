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
#'   * the id's of the tree-ring series ("series")
#'   * the number of sapwood rings observed ("n_sapwood)
#'   * the presence of waney edge ("waneyedge")
#'   * the year assigned to the last measured ring ("last").
#'
#'   Optionally, a column specifying the sapwood data set (`sw_data`) can also be included.
#'
#'
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
#'    [sw_data_overview()],
#'
#'    * or the name of a `data.frame` with sapwood data
#'    in columns `n_sapwood` and `count`, or
#'
#'    * or character string naming a column in `x` that lists for each series
#'    the sapwood model to use, e.g. sw_data = "sapwood_model_column".
#'
#' @param cred_mass A numeric `scalar [0, 1]` specifying the mass within the
#'   credible interval (default = .954).
#' @param densfun Name of the density function to fit to the sapwood
#'   distribution. Should be one of:
#'   * _lognormal_ (the default value),
#'   * _normal_,
#'   * _weibull_,
#'   * _gamma_.
#'
#' @return A `data.frame` with felling date estimates per tree-ring series.
#' Columns include:
#' * `series`: series identifier
#' * `last`: last measured ring
#' * `n_sapwood`: number of sapwood rings
#' * `waneyedge`: TRUE/FALSE for waney edge
#' * `lower`, `upper`: numeric bounds for the estimated felling date
#' * `felling_date`: a character summary (e.g., "between 1500 and 1510")
#' * `sapwood_model`: the sapwood data/model used
#'
#' @examples
#' df <- data.frame(
#'      id = c("trs1", "trs2", "trs3", "trs4"),
#'      swr = c(7, 1, 10, 12),
#'      waneyedge = c(FALSE, FALSE, FALSE, TRUE),
#'      end = c(1482, 1475, 1490, 1498)
#' )
#' fd_report(df,
#'      series = "id",
#'      n_sapwood = "swr",
#'      last = "end",
#'      sw_data = "Wazny_1990"
#' )
#'
#' # Example with different sw_model for individual series
#' # You can add a user-defined sapwood dataset as well.
#'
#' sapwood_model_column <- c(
#'      "Sohar_2012_ELL_c",
#'      "Wazny_1990",
#'      "Hollstein_1980",
#'      "vanDaalen_Norway"
#' )
#'
#' df2 <- cbind(df, sw_data = sapwood_model_column)
#'
#' fd_report(df2,
#'      series = "id",
#'      n_sapwood = "swr",
#'      last = "end",
#'      sw_data = "sw_data"
#' )
#' #' @importFrom rlang .data
#' @export
#' @seealso [sw_interval()], [sw_data_overview()], [sw_interval_plot()]

fd_report <- function(x,
                      series = "series",
                      last = "last",
                      n_sapwood = "n_sapwood",
                      waneyedge = "waneyedge",
                      sw_data = "Hollstein_1980",
                      cred_mass = 0.954,
                      densfun = "lognormal") {
     check_input(x,
          series = series,
          last = last,
          n_sapwood = n_sapwood,
          waneyedge = waneyedge,
          sw_data = sw_data,
          cred_mass = cred_mass,
          densfun = densfun
     )

     series <- x[, series]
     n_sapwood <- x[, n_sapwood]
     last <- x[, last]
     waneyedge <- x[, waneyedge]

     # checks if sw_data is one of the data sets within the package
     if (sw_data %in% sw_data_overview()) {
          sw_data_int <- rep(sw_data, nrow(x))

          # if not, a check if the name refers to a sapwood data set that lives in the local environment
     } else if (exists(sw_data, envir = parent.frame(), inherits = TRUE)) {
          sw_candidate <- get(sw_data, envir = parent.frame(), inherits = TRUE)
          check_sapwood_data_user(sw_candidate)
          sw_data_int <- rep(sw_data, nrow(x))

          # if not, a check whether the name refers to a column in 'x' where for each series
          #  a sapwood model is listed. Checks for valid entries of sapwood data in later called functions
     } else if (sw_data %in% colnames(x)) {
          sw_data_int <- x[, sw_data]
     } else {
          stop("Invalid `sw_data` argument.")
     }

     results <- list()

     for (i in seq_len(nrow(x))) {
          series_i <- series[i]
          n_sapwood_i <- n_sapwood[i]
          last_i <- last[i]
          waneyedge_i <- waneyedge[i]
          sw_data_i <- sw_data_int[i]

          if (waneyedge_i) {
               lower_i <- NA_real_
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
                    # Assume zero sapwood if NA
                    n_sapwood = 0,
                    last = last_i,
                    hdi = TRUE,
                    cred_mass = cred_mass,
                    sw_data = sw_data_i,
                    densfun = densfun
               )
               lower_i <- interval_i[[1]]
               upper_i <- NA_real_
          } else if (is.na(last_i)) {
               lower_i <- NA_real_
               upper_i <- NA_real_
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

          results[[length(results) + 1]] <- data.frame(
               series = series_i,
               last = last_i,
               n_sapwood = n_sapwood_i,
               waneyedge = waneyedge_i,
               lower = lower_i,
               upper = upper_i,
               felling_date = verbal_i,
               sapwood_model = sw_data_i,
               stringsAsFactors = FALSE
          )
     }

     final_result <- do.call(rbind, results)
     attr(final_result, "cred_mass") <- cred_mass
     attr(final_result, "model") <- densfun
     return(final_result)
}
