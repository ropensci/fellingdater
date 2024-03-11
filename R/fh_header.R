#' Retrieve the HEADER fields of a Heidelberg format (.fh) file
#'
#' @description This function reports the HEADER fields from a Heidelberg format
#'   (.fh) ring-width file. The header fields are harvested from the .fh-file by
#'   the `read_fh()` function, which stores the HEADER fields from the .fh file
#'   as attributes of the `data.frame` with the measurement data it returns.
#'
#' @param rwl The output of `read_fh(x, header = TRUE)`, a `data.frame` of class
#'   `rwl`.
#'
#' @return A `data.frame` with 29 header fields.
#' @examples
#' Doel1 <- system.file("extdata", "DOEL1.fh", package = "fellingdateR")
#' Doel1_trs <- read_fh(Doel1, verbose = FALSE)
#' fh_header(Doel1_trs)
#'
#' @export

fh_header <-
  function(rwl) {
    if (!is.data.frame(rwl) || !inherits(rwl, "rwl")) {
      stop("Input should be a data.frame of class 'rwl'")
    }

    attr(rwl, "row.names") <- NULL
    attr(rwl, "po") <- NULL
    attr(rwl, "class") <- NULL
    attr(rwl, "names") <- NULL

    tmp <- attributes(rwl)
    tmp <- data.frame(tmp)

    return(tmp)
  }
