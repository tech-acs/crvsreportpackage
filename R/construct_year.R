#' Add Year Column to Dataset
#'
#' This function takes a dataset with a specified date column and returns
#' the dataset with an additional column `dobyr` which contains the year extracted from the date column.
#'
#' @param dataset A data frame containing at least one column with date values.
#' @param date_col The name of the column containing date values. Default is `"birth1c"`.
#' @param year_col The name of the column containing year values. Default is `"dobyr"`.
#'
#' @return A data frame with an additional column `dobyr` containing the year of the date column.
#' @import dplyr
#' @export
#' @examples
#' df <- data.frame(birth1c = c("1990-05-15", "1985-10-30", "2000-07-22"))
#' new_df <- add_year_column(df)
#' print(new_df)
#'
#' df <- data.frame(event_date = c("1990-05-15", "1985-10-30", "2000-07-22"))
#' new_df <- add_year_column(df, date_col = "event_date")
#' print(new_df)
construct_year <- function(dataset, date_col = "birth1c", year_col = "dobyr") {
  # Ensure the date_col column is in Date format
  dataset <- dataset %>%
    mutate(!!date_col := as.Date(.data[[date_col]], format = "%Y-%m-%d"))

  # Add the year_col column by extracting the year from date_col
  dataset <- dataset %>%
    mutate(!!year_col := as.integer(format(.data[[date_col]], "%Y")))

  return(dataset)
}
