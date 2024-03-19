#' Compute a single felling date range by combining multiple sapwood estimates
#'
#' @description This function assesses whether it is possible to combine
#' multiple sapwood estimates into a single felling date range.
#'
#' @param x A `data.frame` with at least four columns, providing information on
#'   the ID's of the tree-ring series, the number of sapwood rings observed, the
#'   presence of waney edge and the calendar date assigned to the last measured
#'   ring. A column describing the sapwood data set to be used for modelling and
#'   the computation of the hdi can be provided as well.
#' @param series Name of the column in `x` where ID's of the tree-ring series
#'   are listed as `character` values.
#' @param last Name of the column in `x` where calendar years assigned to the
#'   last measured ring are listed (should be `numeric`).
#' @param n_sapwood Name of the column in `x` where the number of observed
#'   sapwood rings are listed (should be `numeric`).
#' @param waneyedge Name of the column in `x` indicating the presence
#'   (`TRUE`)/absence (`FALSE`) of waney edge (should be a `logical` vector).
#' @param sw_data The name of the sapwood data set to use for modelling. It
#'   should be one of the data sets listed in [sw_data_overview()], or the name
#'   of a `data.frame` with sapwood data in columns `n_sapwood` and `count`.
#' @param cred_mass A `scalar [0, 1]` specifying the mass within the credible
#'   interval (default = .954).
#' @param hdi A `logical` parameter. If `TRUE`, the lower and upper limit of the
#'   highest density interval (credible interval) is given for the combined
#'   felling date. When `FALSE`, a matrix is returned with scaled p values for
#'   calendar years covering the combined estimate of the felling date range.
#' @param plot A `logical` parameter. If `TRUE` a ggplot-plot style graph is
#'   returned of the individual and combined estimate of the felling date. If
#'   `FALSE`, a list with numeric output of the modelling process is returned.
#' @param densfun Name of the density function fitted to the sapwood data set.
#'   Should be one of:
#'   * _lognormal_ (the default value),
#'   * _normal_,
#'   * _weibull_,
#'   * _gammma_.
#'
#' @return Depends on the value of plot. If `TRUE` a ggplot-style is returned,
#'   when `FALSE` a `data.frame` with the combined probability ($raw_data$comb),
#'   the modelling parameters ($sapwood_data, $sapwood_model, $cred_mass),
#'   the Highest Posterior Density Interval for combined combined felling date
#'   interval ($hdi_combine), and the Agreement indices for the model ($A_model)
#'   and individual series ($individual_series$agr_index).
#' @export
#'
#' @seealso [sw_combine_plot()]
#'
#' @examples
#' # a data set in which all series have preserved sapwood
#'
# trs_example1
# sw_combine(trs_example1, plot = FALSE)
#' sw_combine(trs_example1, plot = TRUE)
#'
#' # a data set in which one series has an exact felling date (= waney edge preserved)
#'
#' trs_example2
#' sw_combine(trs_example2, plot = TRUE)
#'
#' # a data set in which multiples series have an exact felling date
#'
#' trs_example3
#' sw_combine(trs_example3, plot = FALSE)
#'
sw_combine <- function(x,
                       series = "series",
                       last = "last",
                       n_sapwood = "n_sapwood",
                       waneyedge = "waneyedge",
                       sw_data = "Hollstein_1980",
                       densfun = "lognormal",
                       cred_mass = 0.954,
                       hdi = TRUE,
                       plot = FALSE) {
  check_input(
    x = x,
    series = series,
    last = last,
    n_sapwood = n_sapwood,
    waneyedge = waneyedge,
    sw_data = sw_data,
    cred_mass = cred_mass,
    densfun = densfun
  )

  swr <- x[, n_sapwood]
  end_date <- x[, last]
  cambium <- x[, waneyedge]

  n <- nrow(x)
  time_axis <- seq(min(end_date) - 3, max(end_date) + 100, by = 1)

  pdf_matrix <- matrix(nrow = length(time_axis), ncol = 1)
  colnames(pdf_matrix) <- "year"
  pdf_matrix[, 1] <- time_axis
  keycodes <- x[, series]
  keycodes <- as.character(keycodes)

  hdi_model <- sw_interval(
    n_sapwood = 0,
    last = 0,
    hdi = TRUE,
    cred_mass = cred_mass,
    sw_data = sw_data,
    densfun = densfun
  )
  rownames(hdi_model) <- NULL
  hdi_min <- hdi_model[[1]]
  hdi_max <- hdi_model[[2]]

  # no series with sapwood or waney edge --> compute terminus post quem
  if (all(cambium == FALSE) & all(is.na(swr))) {
    most_recent <- max(end_date)
    tpq <- most_recent + hdi_min

    message <- paste0("earliest possible felling date: ", tpq)

    for (i in seq_len(length(keycodes))) {
      keycode_i <- keycodes[i]
      pdf <- matrix(NA,
        nrow = length(time_axis),
        ncol = 2
      )
      pdf[, 1] <- time_axis
      colnames(pdf) <- c("year", keycode_i)
      end_date_i <- end_date[i]
      pdf[pdf[, "year"] < end_date_i, keycode_i] <- 0
      pdf[pdf[, "year"] >= end_date_i, keycode_i] <- NA
      pdf_matrix <-
        merge(pdf_matrix,
          pdf,
          by = "year",
          all = TRUE
        )
    }

    pdf_matrix <-
      pdf_matrix[rowSums(is.na(pdf_matrix)) != ncol(pdf_matrix) - 1, ]

    # combine probabilities into one probability density function
    if (n == 1) {
      pdf_matrix$comb <- pdf_matrix[, 2]
    } else {
      pdf_matrix$comb <- apply(
        pdf_matrix[, -1],
        1,
        FUN = function(x) {
          prod(x, na.rm = TRUE)
        }
      )
    }

    hdi <- c("lower" = tpq, "upper" = NA_integer_)
    summary <- fd_report(
      x,
      series = series,
      last = last,
      n_sapwood = n_sapwood,
      waneyedge = waneyedge,
      sw_data = sw_data,
      cred_mass = cred_mass,
      densfun = densfun
    )[, 1:6]
    summary$agr_index <- NA
    rownames(summary) <- NULL
    agr_crit <- NA
    agr_model <- NA
  } else {
    # some series with sapwood or waney edge

    pdf_matrix[, 1] <- time_axis
    colnames(pdf_matrix) <- "year"

    for (i in seq_len(length(keycodes))) {
      keycode_i <- keycodes[i]
      swr_i <- swr[i]
      end_date_i <- end_date[i]
      cambium_i <- cambium[i]

      if (cambium_i == TRUE) {
        # exact felling date
        pdf <-
          matrix(NA,
            nrow = length(time_axis),
            ncol = 2
          )
        pdf[, 1] <- time_axis
        colnames(pdf) <- c("year", keycode_i)
        pdf[pdf[, "year"] == end_date_i, keycode_i] <- 1
        pdf[pdf[, "year"] != end_date_i, keycode_i] <- 0
        pdf_matrix <-
          merge(pdf_matrix,
            pdf,
            by = "year",
            all = TRUE
          )
      } else if (is.na(swr_i)) {
        # terminus post quem date
        pdf <-
          matrix(NA,
            nrow = length(time_axis),
            ncol = 2
          )
        pdf[, 1] <- time_axis
        colnames(pdf) <- c("year", keycode_i)
        pdf[pdf[, "year"] < end_date_i, keycode_i] <- 0
        pdf[pdf[, "year"] >= end_date_i, keycode_i] <- NA
        pdf_matrix <-
          merge(pdf_matrix,
            pdf,
            by = "year",
            all = TRUE
          )
      } else {
        #  apply sw_interval to each individual series
        pdf <- sw_interval(
          n_sapwood = swr_i,
          last = end_date_i,
          hdi = FALSE,
          cred_mass = cred_mass,
          sw_data = sw_data,
          densfun = densfun
        )
        # remove column "n_sapwood" from output sw_interval
        pdf <- pdf[c(1, 3)]
        colnames(pdf) <- c("year", keycode_i)
        pdf_matrix <-
          merge(pdf_matrix,
            pdf,
            by = "year",
            all = TRUE
          )
        # fill matrix with 0's when NA
        pdf_matrix[is.na(pdf_matrix[, keycode_i]), keycode_i] <-
          0
      }
    }

    pdf_matrix <-
      pdf_matrix[rowSums(is.na(pdf_matrix)) != ncol(pdf_matrix) - 1, ]

    # combine probabilities into one probability density function
    if (n == 1) {
      pdf_matrix$comb <- pdf_matrix[, 2]
    } else {
      pdf_matrix$comb <- apply(
        pdf_matrix[, -1],
        1,
        FUN = function(x) {
          prod(x, na.rm = TRUE)
        }
      )

      if (any(pdf_matrix[, 2:length(keycodes) + 1] == 1, na.rm = TRUE)) {
        # when multiple exact felling dates are listed that do no correspond
        # check rowwise if there is any p-value == 1, and set comb = 1
        pdf_matrix[apply(
          pdf_matrix[, 2:length(keycodes) + 1] == 1,
          1,
          FUN = function(x) {
            any(x, na.rm = TRUE)
          }
        ), "comb"] <- 1
      } else if (sum(pdf_matrix$comb, na.rm = TRUE) > 0) {
        # avoid division by 0
        pdf_matrix$comb <-
          pdf_matrix$comb / sum(pdf_matrix$comb, na.rm = TRUE)
      }
    }

    if (hdi == FALSE) {
      return(pdf_matrix)
    } else if (hdi == TRUE &
      sum(pdf_matrix$comb, na.rm = TRUE) == 0) {
      message <- paste0("unable to combine these ", n, " series")

      hdi <- c(
        "lower" = NA,
        "upper" = NA
      )

      summary <- fd_report(
        x,
        series = series,
        last = last,
        n_sapwood = n_sapwood,
        waneyedge = waneyedge,
        sw_data = sw_data,
        cred_mass = cred_mass,
        densfun = densfun
      )[, 1:6]
      summary$agr_index <- NA
      rownames(summary) <- NULL

      agr_crit <- NA
      agr_model <- NA
    } else if (hdi == TRUE &
      (sum(pdf_matrix[, "comb"], na.rm = TRUE) >= 2)) {
      # case with multiple and not corresponding felling dates

      fds <- pdf_matrix[pdf_matrix[, "comb"] == 1, "year"]
      fds <- paste0(fds, collapse = ", ")

      message <- paste0("multiple felling dates: ", fds)

      hdi <- c(
        "lower" = NA,
        "upper" = NA
      )

      summary <- fd_report(
        x,
        series = series,
        last = last,
        n_sapwood = n_sapwood,
        waneyedge = waneyedge,
        sw_data = sw_data,
        cred_mass = cred_mass,
        densfun = densfun
      )[, 1:6]
      summary$agr_index <- NA
      rownames(summary) <- NULL

      agr_crit <- NA
      agr_model <- NA
    } else {
      hdi <- hdi(
        pdf_matrix,
        a = "year",
        b = "comb",
        cred_mass = cred_mass
      )

      if (hdi[1] == hdi[2]) {
        message <- paste("exact felling date: ", hdi[2])
      } else {
        message <- paste(
          "felling date range: ",
          hdi[1],
          " - ",
          hdi[2]
        )
      }

      agr_prod <- 1
      agr_index <- matrix(
        nrow = length(keycodes),
        ncol = 1
      )
      dimnames(agr_index) <- list(keycodes, "agr_index")

      for (i in seq_len(length(keycodes))) {
        if (is.na(swr[i]) & cambium[i] == FALSE) {
          agr_index[i, 1] <- NA
        } else {
          temp <- apply(
            pdf_matrix[, c(
              1 + i,
              length(keycodes) + 2
            )],
            1,
            FUN = function(z) {
              prod(z, na.rm = TRUE)
            }
          )
          temp <- sum(temp, na.rm = TRUE)
          temp2 <- apply(
            pdf_matrix[, c(1 + i, 1 + i)],
            1,
            FUN = function(z) {
              prod(z, na.rm = TRUE)
            }
          )
          temp2 <- sum(temp2, na.rm = TRUE)
          agr_index[i, 1] <- round(temp / temp2 * 100, 1)
          # A is the product of all individual A_i values
          agr_prod <- agr_index[i, 1] / 100 * agr_prod
        }
      }

      summary <- fd_report(
        x,
        series = series,
        last = last,
        n_sapwood = n_sapwood,
        waneyedge = waneyedge,
        sw_data = sw_data,
        cred_mass = cred_mass,
        densfun = densfun
      )[, 1:6]
      summary$agr_index <- round(agr_index, 1)
      rownames(summary) <- NULL

      agr_model <-
        round(100 * agr_prod^(1 / sqrt(
          length(keycodes)
        )), 1)

      agr_crit <- 60
    }
  }
  names(agr_model) <- "Overall agreement index (%)"
  names(agr_crit) <- "Critical threshold (%)"


  model_summary <- list(
    raw_data = pdf_matrix,
    sapwood_data = sw_data,
    sapwood_model = densfun,
    cred_mass = cred_mass,
    hdi_model = hdi_model,
    hdi_combine = hdi,
    individual_series = summary,
    A_model = agr_model,
    A_c = agr_crit,
    model_summary = message
  )

  if (plot) {
    sw_combine_plot(model_summary)
  } else {
    return(model_summary)
  }
}
