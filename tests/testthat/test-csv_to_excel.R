library(testthat)
library(purrr)
library(readr)
library(fs)
library(writexl)

test_that("csv_to_excel creates an Excel file with correct worksheets", {
  # Create a temporary directory
  temp_dir <- tempdir()

  # Create sample CSV files in the temporary directory
  write_csv(data.frame(a = 1:3, b = 4:6), file.path(temp_dir, "file1.csv"))
  write_csv(data.frame(x = 7:9, y = 10:12), file.path(temp_dir, "file2.csv"))

  # Run the function
  output_file <- csv_to_excel(temp_dir)

  # Check if the output file exists
  expect_true(file.exists("output.xlsx"))

  # Check the content of the Excel file
  excel_content <- readxl::excel_sheets("output.xlsx")

  # Check that the correct worksheets are created
  expect_true("file1" %in% excel_content)
  expect_true("file2" %in% excel_content)

  # Clean up
  file.remove(file.path(temp_dir, "file1.csv"))
  file.remove(file.path(temp_dir, "file2.csv"))
  file.remove("output.xlsx")
})

test_that("csv_to_excel handles no CSV files gracefully", {
  # Create a temporary directory with no CSV files
  temp_dir <- tempdir()

  # Suppress the warning and run the function
  expect_warning(output_file <- csv_to_excel(temp_dir), "There are no .csv files in this directory")

  # Check that no output file is created
  expect_false(file.exists("output.xlsx"))
})
