% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_table_5.2.R
\name{create_t5.2}
\alias{create_t5.2}
\title{Creates Tables 5.2}
\usage{
create_t5.2(data, date_var, data_year)
}
\arguments{
\item{data}{death registration data frame}

\item{date_var}{event year column of the dataframe like dodyr}

\item{data_year}{the year to report on}
}
\value{
data frame of tabulated results
}
\description{
Table 5.2 Deaths by place of usual residence and sex of decedent
}
\examples{
t5.2 <- create_t5.2(dth_data, date_var=dodyr, data_year = 2022, tablename = "Table_5_2")
}
