# Load required libraries
library(testthat)
library(dplyr)

# Unit Tests
test_that("construct_timeliness computes date differences and categorizes correctly", {
  # Example Data
  df <- data.frame(
    birth1a = as.Date(c('2023-01-01', '2022-01-01', '2020-01-01', '2023-06-01')),
    birth1b = as.Date(c('2023-01-15', '2022-02-15', '2021-06-01', '2024-06-01'))
  )

  # Expected Results
  expected_datac1 <- c(14, 45, 517, 366)
  expected_datac2 <- c("current", "late", "delayed", "delayed")

  # Run the function
  result <- construct_timeliness(df)

  # Check the results
  expect_equal(result$datac1, expected_datac1)
  expect_equal(result$datac2, expected_datac2)
})

test_that("construct_timeliness handles thresholds correctly", {
  # Example Data
  df <- data.frame(
    birth1a = as.Date(c('2023-02-01', '2022-01-01')),
    birth1b = as.Date(c('2023-02-15', '2023-01-01'))
  )

  # Expected Results with different thresholds
  expected_datac2 <- c("current", "delayed")

  # Run the function with custom thresholds
  result <- construct_timeliness(df, threshold_late = 20, threshold_delayed = 30)

  # Check the results
  expect_equal(result$datac2, expected_datac2)
})
