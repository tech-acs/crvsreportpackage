#' Creates Table 4.3
#'
#'Table 4.3 Live births by place of occurrence and place of usual residence of mother
#'
#' @param data data frame being used
#' @param date_var occurrence data being used e.g. dobyr, dodyr etc
#' @param data_year year the data is for
#' @param tablename name for csv output use _ instead of . for names
#' @param output_path The path to export the generated table
#'
#' @return data frame with tablutated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t4.3 <- create_t4.3(bth_data, dobyr, 2022)

create_t4.3 <- function(data, date_var = "dobyr", data_year = NA, tablename = "Table_4_3", output_path = NULL){

  # if data_year is not provided, take the latest year in the data
  if (is.na(data_year)){
    data_year = data %>% pull(!!sym(date_var)) %>% max(na.rm = TRUE)
  }

  output <- data |>
    filter(is.na(birth1j) & !!sym(date_var) == data_year) |>
    group_by(birth1c, birth3l) |>
    mutate(match = if_else(birth1c == birth3l, "Same as place of occurrence", "Other location")) |>
    group_by(birth1c, match) |>
    summarise(total = n()) |>
    pivot_wider(names_from = match, values_from = total, values_fill = 0) |>
    adorn_totals(c("col", "row"))

  if (is.null(output_path)){
    return(output)
  } else {
    write.csv(output, paste0(output_path, tablename, ".csv"), row.names = FALSE)
    return(output)
  }
}



