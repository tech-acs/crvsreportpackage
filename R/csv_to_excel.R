#' Convert CSV files into an Excel spreadsheet
#'
#' @description
#' Reads multiple CSV files and outputs them as individual worksheets within a single Excel spreadsheet.
#'
#' @param path The path to the folder containing the CSV files
#'
#' @return An Excel file (.xlsx) containing worksheets for each imported CSV file
#' @export
#'
#' @import purrr
#' @import readr
#' @import fs
#' @import writexl
#'
#' @examples csv_to_excel(path = ".")
#'
csv_to_excel <- function(path = ".") {
  file_list <- list.files(path = path, pattern = "*.csv", full.names = TRUE)

  if (length(file_list) == 0) {
    warning("There are no .csv files in this directory")
    return(NULL)
  }

  table_list <- file_list %>%
    map(~readr::read_csv(.), .id = "id") %>%
    set_names(tools::file_path_sans_ext(path_file(file_list)))

  excel_file <- write_xlsx(table_list, "output.xlsx")

  return(excel_file)
}
