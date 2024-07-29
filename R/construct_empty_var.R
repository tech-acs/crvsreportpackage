#' Add NA Column to Dataset
#'
#' This function takes a dataset and adds a new column filled with NA values.
#'
#' @param dataset A data frame to which the new column will be added.
#' @param column_name The name of the new column to be added. Default is `"birth1j"`.
#' @return A data frame with the new column added, containing NA values.
#' @export
#' 
#' @import dplyr
#' 
#' @examples
#' df <- data.frame(id = 1:5, name = c("Alice", "Bob", "Charlie", "David", "Eve"))
#' new_df <- construct_empty_var(df)
#' print(new_df)
#' @export
construct_empty_var <- function(dataset, column_name = "birth1j") {
  dataset <- dataset %>%
    mutate(!!column_name := NA)
  
  return(dataset)
}