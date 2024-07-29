#' Create Leading deaths Age Group Column
#'
#' This function takes a dataset and a column containing age in integer form,
#' and creates a new column with age group labels.
#'
#' @param data A data frame containing the dataset.
#' @param age_col The column name (as a string) containing age in integer form.
#' @param age_group_col The new column name for the age group labels (default is "age_grp_lead").
#'
#' @return A data frame with the new age group column added.
#' @export
#'
#' @examples
#' data <- data.frame(death2b = c(3, 8, 45, 72))
#' data <- construct_leading_groups(data, age_col = 'death2b', age_group_col = "age_grp_lead")
#' print(data)
construct_leading_groups <- function(data, age_col = 'death2b', age_group_col = "age_grp_lead") {
  data <- data %>%
    mutate(!!age_group_col := case_when(
      !!sym(age_col) >= 0 & !!sym(age_col) <= 4  ~ "0-4",
      !!sym(age_col) >= 5 & !!sym(age_col) <= 14 ~ "5-14",
      !!sym(age_col) >= 15 & !!sym(age_col) <= 69 ~ "15-69",
      !!sym(age_col) >= 70 ~ "70+",
      TRUE ~ NA_character_
    ))

  return(data)
}
