% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trs_tSt.R
\name{trs_tSt}
\alias{trs_tSt}
\title{Compute Student's t-statistics from correlation coefficients}
\usage{
trs_tSt(x, y = NULL, min_overlap = 30, as_df = FALSE)
}
\arguments{
\item{x}{A data frame of test tree-ring series in \code{rwl} format (years as rownames,
series as columns). All columns must be numeric.}

\item{y}{A data frame of reference tree-ring series in \code{rwl} format.}

\item{min_overlap}{Integer. Minimum number of overlapping years required between
series pairs to compute statistics. Must be >= 3. Default is 30.}

\item{as_df}{Logical. If \code{TRUE}, returns results as data frames. If \code{FALSE},
returns as matrices. Default is \code{TRUE}.}
}
\value{
A list containing:
\describe{
\item{r}{Correlation coefficients between series pairs}
\item{t}{Student's t-statistics}
\item{overlap}{Number of overlapping years between series pairs}
}
}
\description{
This function computes correlation coefficients and their corresponding
Student's t-statistics between all pairs of series from two tree-ring datasets.
}
\details{
The function computes Pearson correlation coefficients between all pairs of
series from the two input datasets, then converts these to Student's t-statistics
using the formula: t = r * sqrt(n-2) / sqrt(1-r²), where n is the number of
overlapping observations.
}
\examples{
# Create sample data,
trs <- trs_pseudo_rwl(n_series = 5, series_length = c(80, 100), end_date = c(2010, 2020))
trs_tSt(trs)$t_St

}
