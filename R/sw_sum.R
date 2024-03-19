#' Compute the summed probability of multiple felling date ranges
#'
#' @param x A `data.frame` with columns c("series", "swr", "waneyedge" ,
#'   "last"), Column "sw_model" is optional. Could be the output of read_fh(x,
#'   header = TRUE)
#' @param series Name of the column in `x` where ID's of the tree-ring series
#'   are listed as `character` values.
#' @param last Name of the column in `x` where calendar years assigned to the
#'   last measured ring are listed (should be `numeric`).
#' @param n_sapwood Name of the column in `x` where the number of observed
#'   sapwood rings are listed (should be `numeric`)
#' @param waneyedge Name of the column in `x` indicating the presence
#'   (`TRUE`)/absence (`FALSE`) of waney edge (should be a `logical`).
#' @param cred_mass A `scalar [0, 1]` specifying the mass within the credible
#'   interval (default = .954).
#' @param sw_data The name of the sapwood data set to use for modelling. It
#'   should be one of the data sets listed in [sw_data_overview()], or the name
#'   of a `data.frame` with sapwood data in columns `n_sapwood` and `count`.
#' @param densfun Name of the density function fitted to the sapwood data set.
#'   Should be one of:
#'   * _lognormal_ (the default value),
#'   * _normal_,
#'   * _weibull_,
#'   * _gammma_.
#' @param plot A `logical`.
#'   * If `TRUE`, [sw_sum_plot()] is triggered and a ggplot-style graph is
#'   returned with the summed probability density (SPD).
#'   * If `FALSE`, a `list` with the numeric output of the modelling process is
#'   returned.
#' @param scale_p A `logical`. If `TRUE` the summed probability density is
#'   scaled to 1 (default).
#'
#' @description Computes the summed probability density (SPD) for a set of
#'   felling date ranges.
#'
#' @return Depends on the value of `plot.`
#' @export
#' @seealso [sw_sum_plot()]
#' @examples
#' trs_example7
#'
#' sw_sum(trs_example7, densfun = "lognormal", cred_mass = 0.63, plot = FALSE)
#'
sw_sum <- function(x,
                   series = "series",
                   last = "last",
                   n_sapwood = "n_sapwood",
                   waneyedge = "waneyedge",
                   sw_data = "Hollstein_1980",
                   densfun = "lognormal",
                   cred_mass = 0.954,
                   plot = FALSE,
                   scale_p = FALSE) {
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
  n_original <- nrow(x)
  x <- dplyr::filter(x, !is.na(n_sapwood) | isTRUE(waneyedge) )

  if (nrow(x) < n_original) {
    warning(
      paste0(
        "--> ",
        n_original - nrow(x),
        " series without sapwood rings or waney edge detected
        and removed from the data set"
      )
    )
  }

  swr <- x[, n_sapwood]
  end_date <- x[, last]
  cambium <- x[, waneyedge]

  if (nrow(x) == 0) {
    stop("--> No series with sapwood or waney edge in dataset")
  }

  keycodes <- x[, series]

  if (sw_data %in% sw_data_overview()) {
    sw_data <- rep(sw_data, nrow(x))
  } else if (exists(sw_data)){
    sw_data <- rep(sw_data, nrow(x))
  } else if (sw_data %in% colnames(x)) {
    sw_data <- x[, sw_data]
  }

  wk_true <- which(cambium)
  wk_true <- keycodes[wk_true]

  time_range <- range(x[, last])
  time_axis <- seq(time_range[1], time_range[2] + 100, by = 1)

  pdf_matrix <- matrix(nrow = length(time_axis), ncol = 1)


  pdf_matrix[, 1] <- time_axis
  colnames(pdf_matrix) <- "year"

  for (i in seq_len(length(keycodes))) {
    keycode_i <- keycodes[i]
    swr_i <- swr[i]
    yr <- end_date[i]
    cambium_i <- cambium[i]
    sw_data_i <- sw_data[i]

    if (cambium_i == TRUE) {
      felling_date <- x[i, last]
      pdf <- matrix(NA, nrow = 1, ncol = 2)
      colnames(pdf) <- c("year", keycode_i)
      pdf[1, 1] <- felling_date
      pdf[1, 2] <- 1
      pdf_matrix <-
        merge(pdf_matrix,
          pdf,
          by = "year",
          all = TRUE
        )
    } else {
      pdf <-
        sw_interval(
          n_sapwood = swr_i,
          last = yr,
          sw_data = sw_data_i,
          densfun = densfun,
          cred_mass= cred_mass
        )
      pdf <- pdf[, -2] # remove column n_sapwood
      colnames(pdf) <- c("year", keycode_i)
      pdf_matrix <-
        merge(pdf_matrix,
          pdf,
          by = "year",
          all = TRUE
        )
    }
  }

  # sum probabilities to SPD
  if (length(wk_true) == 0) {
    tmp <- pdf_matrix[, "year", drop = FALSE]
    tmp$spd_wk <- 0
  } else {
    tmp <- pdf_matrix[, c("year", wk_true), drop = FALSE]
    if (dim(tmp)[2] > 2) {
      tmp$spd_wk <-
        rowSums(tmp[, -1], na.rm = TRUE)
    }
    if (ncol(tmp) == 2) {
      tmp$spd_wk <- tmp[, 2]
    }
    if (ncol(tmp) == 1) {
      tmp$spd_wk <- 0
    }
    tmp <- tmp[, c("year", "spd_wk")]
  }

  tmp2 <-
    pdf_matrix[, setdiff(names(pdf_matrix), wk_true), drop = FALSE]

  if (dim(tmp2)[2] > 2) {
    pdf_matrix$spd <- rowSums(tmp2[, -1], na.rm = TRUE)
  }
  if (dim(tmp2)[2] == 2) {
    pdf_matrix$spd <- tmp2[, 2]
  }
  if (dim(tmp2)[2] < 2) {
    pdf_matrix$spd <- 0
  }

  pdf_matrix <-
    merge(pdf_matrix, tmp, by = "year", all = TRUE)

  # scale SPD to 1
  if (scale_p == TRUE) {
    pdf_matrix$spd <- pdf_matrix$spd / sum(pdf_matrix$spd, na.rm = TRUE)
  }

  if (plot) {
    sw_sum_plot(pdf_matrix)
  } else {
    return(pdf_matrix)
  }
}
