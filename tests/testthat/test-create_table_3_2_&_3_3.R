library(testthat)
library(dplyr)
library(tidyr)
library(janitor)

# Sample data
bth_data <- data.frame(
  birth1j = c(NA, NA, NA, NA, NA),
  doryr = c(2018, 2018, 2019, 2019, 2020),
  dobyr = c(2018, 2017, 2018, 2019, 2020)
)

deaths_data <- data.frame(
  birth1j = c(NA, NA, NA, NA, NA),
  doryr = c(2018, 2018, 2019, 2019, 2020),
  dobyr = c(2018, 2017, 2018, 2019, 2020)
)

# Define the test
test_that("create_t3.2_t3.3 function works correctly for births", {
  result <- create_t3.2_t3.3(bth_data, occ_var = dobyr, topic = "births", tablename = "test_table_3_2")

  # Check the structure and content of the result
  expect_true(is.data.frame(result))
  expect_true("Grand total" %in% result[[1]])
  expect_equal(ncol(result), length(unique(bth_data$doryr)) + 1)
})

test_that("create_t3.2_t3.3 function works correctly for deaths", {
  result <- create_t3.2_t3.3(deaths_data, occ_var = dobyr, topic = "deaths", tablename = "test_table_3_3")

  # Check the structure and content of the result
  expect_true(is.data.frame(result))
  expect_true("Grand total" %in% result[[1]])
  expect_equal(ncol(result), length(unique(deaths_data$doryr)) + 1)
})

test_that("create_t3.2_t3.3 creates CSV output file", {
  create_t3.2_t3.3(bth_data, occ_var = dobyr, topic = "births", tablename = "test_table_3_2")
  create_t3.2_t3.3(deaths_data, occ_var = dobyr, topic = "deaths", tablename = "test_table_3_3")

  expect_true(file.exists("./outputs/test_table_3_2.csv"))
  expect_true(file.exists("./outputs/test_table_3_3.csv"))

  # Clean up the files after testing
  file.remove("./outputs/test_table_3_2.csv")
  file.remove("./outputs/test_table_3_3.csv")
})
