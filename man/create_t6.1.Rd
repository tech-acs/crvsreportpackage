% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_table_6_1.R
\name{create_t6.1}
\alias{create_t6.1}
\title{Create Table 6.1}
\usage{
create_t6.1(
  data,
  cause,
  date_var = "dodyr",
  data_year = NA,
  tablename = "Table_6_1",
  output_path = NULL
)
}
\arguments{
\item{data}{data frame being used}

\item{cause}{cause of death data frame}

\item{date_var}{column containing the year of event occurence}

\item{data_year}{year of data}

\item{tablename}{name of the table being saved as a csv file}

\item{output_path}{The path to export the generated table}
}
\value{
Returns data frame with tabulated results
}
\description{
Table 6.1 Leading causes of death by broad age group and sex
}
\examples{
t6.1 <- create_t6.1(data = dth_data, cause = cause_dict, date_var = dodyr, data_year = 2022)

}
