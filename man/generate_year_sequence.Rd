% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_year_sequence.R
\name{generate_year_sequence}
\alias{generate_year_sequence}
\title{Generate year sequence}
\usage{
generate_year_sequence(latest_year, num_yrs = 5)
}
\arguments{
\item{latest_year}{The most recent year in the sequence. Must be a numeric value representing a year, such as 2023.}

\item{num_yrs}{The number of years to include in the sequence, counting backwards from the latest year. Default is 5.}
}
\value{
numeric vector containing the sequence of years
}
\description{
The generate_year_sequence function generates a sequence of consecutive years,
starting with the most recent year provided as input. The sequence includes
the specified number of years, counting backwards from the latest year given.
}
\examples{
generate_year_sequence(2023, 5)
}
