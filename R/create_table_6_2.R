#' Create Table 6.2
#'
#' Table 6.2 Top 10 causes of death (all ages, both sexes)
#'
#' @param data data frame being used
#' @param date_var year of data: dodyr or doryr
#' @param cause Dictionary with the causes of deaths
#' @param tablename name of the table being saved as a csv file
#' @param output_path The path to export the generated table
#'
#' @return Returns data frame with tabulated results
#' @export
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples
#' t6.2 <- create_t6.2(dth_data, cause)
create_t6.2 <- function(death_data, cause, date_var = 'dodyr', tablename = "Table_6_1", output_path = NULL){
  # Join death_data with cause descriptions
  death_data <- death_data %>%
    left_join(cause, by = c("death1g" = "code"))

  # Filter out empty death causes
  death_data <- death_data %>%
    filter(death1g != "")

  # Group by year and cause, then count occurrences
  death_summary <- death_data %>%
    group_by(!!sym(date_var), description) %>%
    summarise(count = n(), .groups = 'drop')

  # Get the top 10 causes for each year
  top_causes <- death_summary %>%
    group_by(!!sym(date_var)) %>%
    slice_max(order_by = count, n = 10, with_ties = FALSE) %>%
    arrange(!!sym(date_var), desc(count))

  # Spread the data to wide format
  ranked_table <- top_causes %>%
    group_by(dodyr) %>%
    mutate(rank = row_number()) %>%
    select(dodyr, rank, description) %>%
    pivot_wider(names_from = dodyr, values_from = description) %>%
    arrange(rank)

  # Shift cells up to remove NA gaps
  shift_up <- function(column) {
    non_na_values <- column[!is.na(column)]
    na_values <- rep(NA, length(column) - length(non_na_values))
    c(non_na_values, na_values)
  }

  final_output <- ranked_table %>%
    mutate(across(-rank, shift_up))

  if (is.null(output_path)){
    return(final_output)
  } else {
    write.csv(final_output, paste0(output_path, tablename, ".csv"), row.names = FALSE)
    return(final_output)
  }
}

