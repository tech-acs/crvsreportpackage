% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_table_7_2_&_7_3.R
\name{create_t7.2_and_7.3}
\alias{create_t7.2_and_7.3}
\title{Create Tables 7.2 and 7.3}
\usage{
create_t7.2_and_7.3(data, data_year, ruindicator = "urban", tablename = NA)
}
\arguments{
\item{data}{data frame being used}

\item{data_year}{year the data is for}

\item{ruindicator}{whether the data is for urban or rural}

\item{tablename}{name of the table being saved as a csv file}
}
\value{
data frame of tabulated results
}
\description{
Create Tables 7.2 and 7.3
}
\examples{
t7.2 <- create_t7.2_and_7.3(marr_data, data_year = 2020, ruindicator = "urban", tablename = "Table_7_1")
t7.3 <- create_t7.2_and_7.3(marr_data, data_year = 2020, ruindicator = "rural", tablename = "Table_7_2")

}
