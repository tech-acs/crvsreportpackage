#' Create Table 6.1
#'
#' Table 6.1 Leading causes of death by broad age group and sex
#'
#' @param data data frame being used
#' @param cause cause of death data frame
#' @param date_var column containing the year of event occurence
#' @param data_year year of data
#' @param tablename name of the table being saved as a csv file
#' @param output_path The path to export the generated table
#'
#' @return Returns data frame with tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples
#' t6.1 <- create_t6.1(data = dth_data, cause = cause_dict, date_var = dodyr, data_year = 2022)
#'
create_t6.1 <- function(data, cause, date_var = "dodyr", data_year = NA, tablename = "Table_6_1", output_path = NULL){
  # If data_year is not provided, take the latest year in the data
  if (is.na(data_year)){
    data_year <- data %>% pull(!!sym(date_var)) %>% max(na.rm = TRUE)
  }

  # Filter and summarize data
  output <- data %>%
    filter(!!sym(date_var) == data_year & death1g != "" & death2c %in% c("male", "female")) %>%
    mutate(causecd = substr(death1g, 1, 3)) %>%
    group_by(death2c, causecd, age_grp_lead) %>%
    summarise(total = n(), .groups = 'drop')

  # Pivot wider to create columns for male and female
  output <- output %>%
    pivot_wider(names_from = death2c, values_from = total, values_fill = list(total = 0)) %>%
    arrange(age_grp_lead)

  # Ensure the cause data frame has unique cause codes
  cause <- cause %>% distinct(code, .keep_all = TRUE)

  # Join with cause descriptions
  output <- left_join(output, cause, by = c("causecd" = "code"))

  # Extract top 5 causes for each age group and sex
  get_top_causes <- function(output, sex_col) {
    output %>%
      select(description, age_grp_lead, !!sym(sex_col)) %>%
      group_by(age_grp_lead, description) %>%
      summarise(total = sum(!!sym(sex_col)), .groups = 'drop') %>%
      group_by(age_grp_lead) %>%
      slice_max(order_by = total, n = 5, with_ties = FALSE) %>%
      mutate(rank = row_number()) %>%
      arrange(age_grp_lead, rank)
  }

  output_male <- get_top_causes(output, "male")
  output_female <- get_top_causes(output, "female")

  # Combine male and female results by age group and rank
  final_output <- full_join(output_male, output_female, by = c("age_grp_lead", "rank"), suffix = c("_male", "_female")) %>%
    select(age_grp_lead, description_male, total_male, description_female, total_female) %>%
    rename(
      male_cause = description_male,
      deaths_male = total_male,
      female_cause = description_female,
      deaths_female = total_female
    )

  if (is.null(output_path)){
    return(final_output)
  } else {
    write.csv(final_output, paste0(output_path, tablename, ".csv"), row.names = FALSE)
    return(final_output)
  }
}
