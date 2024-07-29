#' Construct Age Group Column to Dataset
#'
#' This function takes a dataset and adds a new column that contains age groups.
#'
#' @param dataset A data frame containing at least one column with age values.
#' @param age_col The name of the column containing age values.
#' @param group_size An integer specifying the size of the age groups. Default is 5.
#' @param new_col The name of the new column to store the age groups. Default is "fert_age_grp".
#' @return A data frame with the new column added, containing age groups.
#'
#' @export
#' @import dplyr
#'
#' @examples
#' df <- data.frame(id = 1:10, age = c(23, 45, 34, 56, 29, 12, 67, 44, 33, 55))
#' new_df <- construct_age_group(df, age_col = "age", group_size = 10)
#' print(new_df)
construct_age_group <- function(dataset, age_col, group_size = 5, new_col = "fert_age_grp") {
  max_age <- max(dataset[[age_col]], na.rm = TRUE)
  breaks <- seq(0, max_age + group_size, by = group_size)
  labels <- paste0(head(breaks, -1), "-", tail(breaks, -1) - 1)

  dataset <- dataset %>%
    mutate(!!new_col := cut(.data[[age_col]],
                            breaks = breaks,
                            include.lowest = TRUE,
                            right = FALSE,
                            labels = labels))

  return(dataset)
}
