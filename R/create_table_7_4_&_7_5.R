#' Create Tables 7.4 and 7.5
#'
#' @description
#' Table 7.4 Marriages by age of groom and previous marital status
#' @description
#' Table 7.5 Marriages by age of bride and previous marital status
#'
#' @param data dataframe being used
#' @param data_year year the data is for
#' @param groombride whether the table is for groom or bride data
#' @param tablename name of the table being saved as a csv file
#'
#' @return data frame of tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples
#' t7.4 <- create_table_7.4_and_7.5(marr_data, data_year = 2020, groombride = "groom", tablename = "Table_7_4")
#' t7.5 <- create_table_7.4_and_7.5(marr_data, data_year = 2020, groombride = "bride", tablename = "Table_7_5")
#'
create_table_7.4_and_7.5 <- function(data, data_year = NA, groombride = "groom", tablename = NA){

  # if data_year is not provided, take the latest year in the data
  if (is.na(data_year)){
    data_year = data %>% pull(!!sym(date_var)) %>% max(na.rm = TRUE)
  }

if(groombride == "groom"){
output <- data |>
  filter(year == data_year) |>
  group_by(g_age_grp, marcongt) |>
  summarise(total = n()) |>
  pivot_wider(names_from = marcongt, values_from = total, values_fill = 0) |>
  adorn_totals(c("row", "col"))
} else {
  output <- data |>
    filter(year == data_year) |>
    group_by(b_age_grp, marconbt) |>
    summarise(total = n()) |>
    pivot_wider(names_from = marconbt, values_from = total, values_fill = 0) |>
    adorn_totals(c("row", "col"))
}


return(output)
}


