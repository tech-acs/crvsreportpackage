# Load necessary package
library(dplyr)

test_that("create_t6.1 function works correctly", {
  working_dir <- getwd()
  setwd("../../")

  # Larger sample data
  set.seed(123) # For reproducibility

  dth_data <- data.frame(
    dodyr = sample(2019:2022, 200, replace = TRUE),
    death1g = sample(c("A01", "B02", "C03", "D04", "E05", "F06", "G07", "H08", "I09", "J10", ""), 200, replace = TRUE),
    death2c = sample(c("male", "female", "not stated"), 200, replace = TRUE),
    age_grp_lead = sample(c("0-4", "5-14", "15-69", "70+"), 200, replace = TRUE)
  )

  cause <- data.frame(
    code = c("A01", "B02", "C03", "D04", "E05", "F06", "G07", "H08", "I09", "J10"),
    description = c("Cause A01", "Cause B02", "Cause C03", "Cause D04", "Cause E05", "Cause F06", "Cause G07", "Cause H08", "Cause I09", "Cause J10")
  )

  # Ensure the cause data frame has unique cause codes
  cause <- cause %>% distinct(code, .keep_all = TRUE)

  # Run the function
  output <- create_t6.1(data = dth_data, cause = cause, date_var = "dodyr", data_year = 2022, tablename = "Table_6_1", output_path = "outputs/")

  # Check if output is a data frame
  expect_s3_class(output, "data.frame")

  # Check if the output contains the correct columns
  expected_columns <- c("age_grp_lead", "male_cause", "deaths_male", "female_cause", "deaths_female")
  expect_true(all(expected_columns %in% colnames(output)))

  # Check if the causes are correctly described
  expect_true(all(output$male_cause %in% cause$description | is.na(output$male_cause)))
  expect_true(all(output$female_cause %in% cause$description | is.na(output$female_cause)))

  # Check if the file is written
  expect_true(file.exists("outputs/Table_6_1.csv"))

  # Clean up the generated file after the test
  unlink("outputs/Table_6_1.csv")

  # Reset working dir
  setwd(working_dir)
})
