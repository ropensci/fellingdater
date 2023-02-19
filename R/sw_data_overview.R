#' sw_data_overview: provides an overview all sapwood data sets that are
#'   available within the `fellingdateR` package
#'
#' @description This function provides an overview of available sapwood data
#'  and models in the `fellingdateR` package.
#'
#' @return A `character` vector with the names of all available data sets
#'  with sapwood counts.
#'
#' @export
#'
#' @examples
#' sw_data_overview()
#'
sw_data_overview <- function() {

     tmp <- utils::data(package = "fellingdateR")
     tmp <- tmp$results
     tmp[, 3]
}
