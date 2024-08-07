% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_table_3_5_&_3_7.R
\name{create_t3.5_and_3.7}
\alias{create_t3.5_and_3.7}
\title{Creates Tables 3.5 or 3.7}
\usage{
create_t3.5_and_3.7(
  data,
  est_data,
  date_var,
  data_year = NA,
  topic = NA,
  tablename = NA
)
}
\arguments{
\item{data}{dataframe being used}

\item{est_data}{data frame of estimated data being used}

\item{date_var}{occurrence data being used e.g. dobyr, dodyr etc}

\item{data_year}{year the data is for}

\item{topic}{whether the data is for births or deaths}

\item{tablename}{name of the table being saved as a csv file}
}
\value{
data frame of tabulated results
}
\description{
Table 3.5 Birth registration completeness by place of usual residence of mother and sex of newborn

Table 3.7 Death registration completeness by place of usual residence and sex of decedent
}
\examples{
t3.5 <- create_t3.5_and_3.7(bth_data, bth_est, dobyr, 2022, topic = "births", tablename = "Table_3_5")

}
