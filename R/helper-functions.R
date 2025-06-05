#' Helper function to scale a Probability Density Function (PDF) to a
#' Probability Frequency Function.
#' @noRd
d_dens <- function(densfun = densfun,
                   x = x,
                   param1 = 0,
                   param2 = 1,
                   log = FALSE,
                   n = 1) {
     if (!densfun %in% c("lognormal", "normal", "weibull", "gamma")) {
          stop(sprintf(
               "!!! '%s' is not a supported distribution !!!",
               densfun
          ))
     }
     if (densfun == "lognormal") {
          n * stats::dlnorm(
               x = x,
               meanlog = param1,
               sdlog = param2,
               log = log
          )
     } else if (densfun == "normal") {
          n * stats::dnorm(
               x = x,
               mean = param1,
               sd = param2,
               log = log
          )
     } else if (densfun == "weibull") {
          n * stats::dweibull(
               x = x,
               shape = param1,
               scale = param2,
               log = log
          )
     } else if (densfun == "gamma") {
          n * stats::dgamma(
               x = x,
               shape = param1,
               rate = param2,
               log = log
          )
     }
}


#' Helper function to rescale probabilities between [0, 1].
#' @noRd
rescale <- function(x,
                    floor = 0,
                    ceiling = 1) {
     if (max(x, na.rm = TRUE) == 0) {
          x
     } else {
          (x - min(x, na.rm = TRUE)) * (ceiling - floor) /
               (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
     }
}



#' Helper function to check input for sw_combine, fd_report and sw_sum
#' @noRd

check_input <- function(x = x,
                        series = series,
                        last = last,
                        n_sapwood = n_sapwood,
                        waneyedge = waneyedge,
                        sw_data = sw_data,
                        cred_mass = cred_mass,
                        densfun = densfun) {
     if (!is.data.frame(x)) {
          stop("Input 'x' must be a dataframe.")
     }

     # Check columns exist
     if (!series %in% names(x)) {
          stop("--> column 'series' does not exist")
     }
     if (!last %in% names(x)) {
          stop("--> column 'last' does not exist")
     }
     if (!n_sapwood %in% names(x)) {
          stop("--> column 'n_sapwood' does not exist")
     }
     if (!waneyedge %in% names(x)) {
          stop("--> column 'waneyedge' does not exist")
     }
     series <- x[, series] # check for NA's
     if (any(is.na(series))) {
          stop("--> some 'series' have no id")
     }

     if (is.character(x[, n_sapwood])) {
          # was !is.numeric !!!
          stop("--> 'n_sapwood' must be a numeric vector")
     }

     if (!is.numeric(x[, last])) {
          stop("--> 'last' must be a numeric vector")
     }

     if (!is.logical(x[, waneyedge])) {
          # check is.logical
          stop("--> 'waneyedge' should be a logical vector (TRUE/FALSE),
indicating the presence of waney edge.\n")
     }

     if (is.na(cred_mass) || cred_mass <= 0 || cred_mass >= 1) {
          stop("--> cred_mass must be between 0 and 1")
     }

     if (!is.character(sw_data)) {
          stop("--> sw_data should be one of `sw_data_overview()`
         or the name a data.frame with numeric values in columns
         `n_sapwood` and `count`.)")
     }

     if (all(
          !sw_data %in% sw_data_overview(),
          !exists(sw_data),
          !sw_data %in% colnames(x)
     )) {
          stop(
               sprintf(
                    "--> sw_data should be one of `sw_data_overview()`
or the name a data.frame with columns `n_sapwood` and `count`, not '%s'.",
                    sw_data
               )
          )
     }

     if (!densfun %in% c("lognormal", "normal", "weibull", "gamma")) {
          stop(
               sprintf(
                    "\n'%s' is not a supported distribution.
          \n`densfun` must be one of c('lognormal', 'normal', 'weibull', 'gamma')",
                    densfun
               )
          )
     }
}

#' Helper function to define a custom theme for plotting in ggplot
#' @noRd
theme_fdr <- function() {
     ggplot2::theme_minimal() +
          ggplot2::theme(
               plot.title = ggtext::element_markdown(),
               plot.title.position = "plot",
               axis.text = ggplot2::element_text(size = 10),
               panel.grid.minor.y = ggplot2::element_blank(),
               panel.grid.major.y = ggplot2::element_blank(),
               legend.position = "none",
               strip.background = ggplot2::element_blank(),
               strip.text.y = ggplot2::element_blank()
          )
}


#' Helper function to check input denfun
#' @noRd
check_densfun <- function(x) {
     if (!x %in% c("lognormal", "normal", "weibull", "gamma")) {
          stop(
               sprintf(
                    "\n'%s' is not a supported distribution.
          \n`densfun` must be one of c('lognormal', 'normal', 'weibull', 'gamma')",
                    x
               )
          )
     }
}

#' Helper function to check input plot
#' @noRd
check_plot <- function(x) {
     if (!is.logical(x)) {
          stop(
               sprintf(
                    "'plot' should be TRUE or FALSE, not '%s'",
                    x
               )
          )
     }
}

#' Helper function to check input cred_mass
#' @noRd
check_cred_mass <- function(x) {
     if (is.na(x) || x <= 0 || x >= 1) {
          stop("--> cred_mass must be between 0 and 1")
     }
}


#' Helper function to check input n_sapwood
#' @noRd
check_n_sapwood <- function(x) {
     if (is.na(x)) {
          message("--> n_sapwood must be a postitive numeric value, not 'NA'")
          return(NA_integer_)
     }

     if (!is.numeric(x)) {
          stop("--> n_sapwood must be a positive numeric value")
     }

     if (x < 0) {
          stop("--> n_sapwood must be a positive numeric value")
     }

     if (isTRUE(x %% 1 != 0)) {
          stop("--> n_sapwood must be an integer (no decimals allowed!)")
     }
}

#' Check if user-supplied sapwood data is valid
#'
#' This function validates a user-defined sapwood dataset by ensuring that it is a
#' `data.frame` with numeric `n_sapwood` and `count` columns, and that all
#' `n_sapwood` values are strictly positive.
#'
#' @param data An object to be tested (typically passed as `sw_data` in `sw_model()`).
#'
#' @return Invisibly returns `TRUE` if all checks pass. Otherwise, an error is thrown.
#'
#' @keywords internal
#' @noRd

check_sapwood_data_user <- function(data) {
     if (!is.data.frame(data)) {
          stop("User-defined sapwood data must be a data.frame.")
     }

     required_cols <- c("n_sapwood", "count")
     if (!all(required_cols %in% names(data))) {
          stop("User-defined data.frame must contain columns: `n_sapwood` and `count`.")
     }

     if (!is.numeric(data$n_sapwood) || !is.numeric(data$count)) {
          stop("Columns `n_sapwood` and `count` must both be numeric.")
     }

     if (anyNA(data$n_sapwood) || anyNA(data$count)) {
          stop("--> Columns `n_sapwood` and `count` must not contain NA values.")
     }

     if (any(data$n_sapwood <= 0)) {
          invalid_vals <- unique(data$n_sapwood[data$n_sapwood <= 0])
          stop(
               "All `n_sapwood` values must be strictly positive. Invalid values found: ",
               paste(invalid_vals, collapse = ", ")
          )
     }

     invisible(TRUE)
}
