% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trs_select.R
\name{trs_select}
\alias{trs_select}
\title{Select series from an rwl-style data frame}
\usage{
trs_select(x, series, rownames_to_years = FALSE, trim = FALSE)
}
\arguments{
\item{x}{A \code{data.frame} of class \code{"rwl"}, where each column is a tree-ring width series and rownames represent years.}

\item{series}{A character string or vector specifying the name(s) of the series (columns) to extract.}

\item{rownames_to_years}{Logical. If \code{TRUE}, the rownames of \code{x} (years) are added as a column named \code{years}.}

\item{trim}{Logical. If \code{TRUE}, leading and trailing all-NA rows are removed using \code{trs_trim()}.}
}
\value{
A \code{data.frame} with the selected series. If \code{rownames_to_years = TRUE}, the result will have an additional \code{year} column.
If \code{trim = TRUE}, leading and trailing all-NA rows are removed. If a single series is selected, returns a single-column data frame.
}
\description{
Extracts one or more tree-ring series (columns) from a data frame of class \code{"rwl"}.
Optionally adds the rownames (assumed to be years) as a column.
}
\examples{
rwl <- trs_pseudo_rwl(n_series = 5, series_length = 60, end_date = 2000)

# Select single series
trs_select(rwl, series = "trs_1")
trs_select(rwl, series = "trs_2", rownames_to_years = TRUE)

# Select multiple series
trs_select(rwl, series = c("trs_1", "trs_2"))
trs_select(rwl, series = c("trs_1", "trs_3", "trs_5"), rownames_to_years = TRUE)

}
