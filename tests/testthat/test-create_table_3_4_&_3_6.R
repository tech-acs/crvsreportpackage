library(testthat)
library(dplyr)
library(tidyr)
library(janitor)

# Sample data for births
bth_data <- data.frame(
  birth1j = c(NA, NA, NA, NA, NA),
  dobyr = c(2018, 2018, 2019, 2019, 2020),
  birth2a = c("male", "female", "male", "female", "male")
)

# Sample data for estimated births
bth_est <- data.frame(
  year = c(2022, 2022, 2022, 2022),
  age_grp = c("0-4", "5-9", "10-14", "15-19"),
  female = c(100, 200, 300, 400),
  male = c(150, 250, 350, 450)
)

# Define the test
test_that("create_t3.4_and_3.6 function works correctly for births", {
  result <- create_t3.4_and_3.6(bth_data, bth_est, by_var = dobyr, topic = "births", tablename = "test_table_3_4")

  # Check the structure and content of the result
  expect_true(is.data.frame(result))
  expect_true("completeness_male" %in% names(result))
  expect_true("completeness_female" %in% names(result))
  expect_true("completeness_total" %in% names(result))
  expect_equal(ncol(result), 10) # by_var, total_male, total_female, total_est_male, total_est_female, completeness_male, completeness_female, total_total, total_est_total, completeness_total
})

test_that("create_t3.4_and_3.6 creates CSV output file", {
  create_t3.4_and_3.6(bth_data, bth_est, by_var = dobyr, topic = "births", tablename = "test_table_3_4")

  expect_true(file.exists("./outputs/test_table_3_4.csv"))

  # Clean up the file after testing
  file.remove("./outputs/test_table_3_4.csv")
})
