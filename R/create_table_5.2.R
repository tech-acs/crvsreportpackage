#' Creates Tables 5.2
#'
#' Table 5.2 Deaths by place of usual residence and sex of decedent
#'
#' @param data death registration data frame
#' @param date_var event year column of the dataframe like dodyr
#' @param data_year the year to report on
#'
#' @return data frame of tabulated results
#' @export
#'
#' @examples
#' t5.2 <- create_t5.2(dth_data, date_var=dodyr, data_year = 2022, tablename = "Table_5_2")

create_t5.2 <- function(data, date_var, data_year = NA, tablename = "Table_5_2"){

  # if data_year is not provided, take the latest year in the data
  if (is.na(data_year)){
    data_year = data %>% pull(!!sym(date_var)) %>% max(na.rm = TRUE)
  }

outputa <- data |>
  #filter({{date_var}} == data_year & substr(birth1c,1,1) %in% c("E","W")) |>
  filter(dodyr == data_year & substr(birth1c,1,1) %in% c("E","W")) |>
  group_by(birth2a, death2r) |>
  summarise(total = n()) |>
  pivot_wider(names_from = birth2a, values_from = total, values_fill = 0) |>
  adorn_totals("col") |>
  rename(area = death2r)

outputb <- data |>
  filter(dodyr == data_year & substr(birth1c,1,1) %in% c("E","W")) |>
  group_by(birth2a, birth1c) |>
  summarise(total = n()) |>
  pivot_wider(names_from = birth2a, values_from = total, values_fill = 0) |>
  adorn_totals("col")|>
  rename(area = birth1c)

output <- rbind(outputa, outputb)

return(output)
}
