#' Creates Table 3.4 and 3.6
#'
#' @param data data frame being used
#' @param est_data data frame of estimated data being used
#' @param by_var the occurrence year being used e.g. dobyr or dodyr
#' @param topic whether the table is births or deaths data
#' @param tablename name of the table being saved as a csv file
#'
#' @return data frames for tabulated versions of Table 3.4 and 3.6
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t3.4 <- create_t3.4_to_3.7(bth_data, bth_est, dobyr, topic = "births", tablename = "Table_3_4")
create_t3.4_and_3.6 <- function(data, est_data, by_var, topic = NA, tablename = NA) {
  max_value <- data %>% pull({{by_var}}) %>% max(na.rm = TRUE)

  counts <- data |>
    filter(({{by_var}}) %in% generate_year_sequence(max_value) &
             if (tolower(topic) == "births") is.na(birth1j) else TRUE) |>
    group_by({{by_var}}, birth2a) |>
    summarise(total = n())

  ests <- est_data |>
    pivot_longer(cols = c("male", "female"), names_to = "birth2a", values_to = "count") |>
    group_by(year, birth2a) |>
    summarise(total_est = sum(count))

  output <- left_join(counts, ests, by= c("dobyr" = "year", "birth2a" = "birth2a"))

  output <- output %>%
    mutate(completeness = round_excel((total / total_est) * 100, 2)) %>%
    pivot_wider(names_from = birth2a, values_from = c(total, total_est, completeness)) %>%
    replace_na(list(total_female = 0, total_male = 0, total_est_female = 0, total_est_male = 0, completeness_female = 0, completeness_male = 0)) %>%
    mutate(
      total_total = total_female + total_male,
      total_est_total = total_est_female + total_est_male,
      completeness_total = round_excel((total_total / total_est_total) * 100, 2)
    ) %>%
    adorn_totals("row", name = "Grand total")

  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)
}
