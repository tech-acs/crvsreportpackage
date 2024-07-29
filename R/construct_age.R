#' Construct and add Age Column to Dataset
#'
#' This function takes a dataset with a specified date of birth column and returns
#' the dataset with an additional column which contains the calculated age.
#'
#' @param dataset A data frame containing at least one column with date of birth values.
#' @param dob_col The name of the column containing date of birth values.
#' @param age_col The name of the new column to store the calculated age. Default is `"age"`.
#' @param end_date_col Optional. The name of the column containing end date values. If not provided, today's date is used.
#' 
#' @return A data frame with an additional column containing the calculated age.
#' @export
#' 
#' @import dplyr
#' @import lubridate
#' 
#' @examples
#' df <- data.frame(death2a = as.Date(c("1990-05-15", "1985-10-30", "2000-07-22")),
#'                  death1a = as.Date(c("2022-07-15", "2022-07-15", "2022-07-15")))
#' new_df <- construct_age(df, dob_col = "death2a", age_col = "death2b", end_date_col = "death1a")
#' print(new_df)
#' 
#' df <- data.frame(birth3a = as.Date(c("1990-05-15", "1985-10-30", "2000-07-22")))
#' new_df <- construct_age(df, dob_col = "birth3a")
#' print(new_df)
construct_age <- function(dataset, dob_col, age_col = "birth3b", end_date_col = NULL) {
  
  # Ensure the dob_col column is in Date format
  dataset <- dataset %>%
    mutate(!!dob_col := as.Date(.data[[dob_col]]))
  
  # Calculate the age
  if (!is.null(end_date_col)) {
    # Ensure the end_date_col column is in Date format
    dataset <- dataset %>%
      mutate(!!end_date_col := as.Date(.data[[end_date_col]]))
    
    dataset <- dataset %>%
      mutate(!!age_col := as.integer((.data[[end_date_col]] - .data[[dob_col]]) / 365.25))
  } else {
    dataset <- dataset %>%
      mutate(!!age_col := as.integer((Sys.Date() - .data[[dob_col]]) / 365.25))
  }
  
  return(dataset)
}
