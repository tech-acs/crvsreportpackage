#' Compute Date Differences and Categorize
#'
#' This function takes a dataset with date variables `birth1a` and `birth1b` and computes
#' two new variables: `datac1`, which is the difference in days between `birth1b` and `birth1a`,
#' and `datac2`, which categorizes this difference into "current", "late", or "delayed".
#'
#' @param data A data frame containing the columns `birth1a` and `birth1b` of date type.
#' @param threshold_late A days threshold to determine whether a registration is current or late. 30 by default.
#' @param threshold_delayed A days threshold to determine whether a registration is delayes. 365 by default.
#'
#' @return A data frame with the original columns plus `datac1` and `datac2`.
#' @import dplyr
#' @export
#' @examples
#' df <- data.frame(
#'   birth1a = as.Date(c('2023-01-01', '2022-01-01', '2020-01-01', '2023-06-01')),
#'   birth1b = as.Date(c('2023-01-15', '2022-02-15', '2021-06-01', '2024-06-01'))
#' )
#' df <- construct_timeliness(df)
construct_timeliness <- function(data, threshold_late = 30, threshold_delayed = 365) {
  data <- data %>%
    mutate(
      datac1 = as.numeric(difftime(birth1b, birth1a, units = "days")),
      datac2 = case_when(
        datac1 < threshold_late ~ "current",
        datac1 >= threshold_late & datac1 <= threshold_delayed ~ "late",
        datac1 > threshold_delayed ~ "delayed",
        TRUE ~ NA_character_  # Handle any other unexpected cases
      )
    )
  return(data)
}
