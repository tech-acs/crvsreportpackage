library(dplyr)

test_that("create_t6.2 function works correctly", {
  working_dir <- getwd()
  setwd("../../")

  # Sample data for testing
  set.seed(123)
  death_data <- data.frame(
    dodyr = sample(2019:2022, 100, replace = TRUE),
    death1g = sample(c("A01", "B02", "C03", "D04", "E05", "F06", "G07", "H08", "I09", "J10", "U11", ""), 100, replace = TRUE),
    death2c = sample(c("male", "female", "not stated"), 100, replace = TRUE)
  )

  cause <- data.frame(
    code = c("A01", "B02", "C03", "D04", "E05", "F06", "G07", "H08", "I09", "J10"),
    description = c("Cause A01", "Cause B02", "Cause C03", "Cause D04", "Cause E05", "Cause F06", "Cause G07", "Cause H08", "Cause I09", "Cause J10")
  )

  # Run the function without output path
  result <- create_t6.2(death_data, cause, date_var = "dodyr")

  # Check if the result is a data frame
  expect_s3_class(result, "data.frame")

  # Check if the result has the correct columns
  expect_true(all(c("rank", as.character(unique(death_data$dodyr))) %in% colnames(result)))

  # Check if the result has at most 10 rows for each year
  years <- unique(death_data$dodyr)
  for (year in years) {
    expect_lte(nrow(result %>% select(starts_with(as.character(year)))), 10)
  }

  # Check if the causes in the result are correctly described
  expect_true(all(unlist(result[, -1], use.names = FALSE) %in% c(cause$description, NA_character_)))

  # Run the function with output path
  result_with_path <- create_t6.2(death_data, cause, date_var = "dodyr", tablename = "Table_6_2", output_path = "outputs/")

  # Check if the file is written
  expect_true(file.exists("outputs/Table_6_2.csv"))

  # Clean up the generated file after the test
  unlink("outputs/Table_6_2.csv")

  # Reset working dir
  setwd(working_dir)
})

