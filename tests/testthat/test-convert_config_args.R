library(testthat)
library(rlang)  # For the sym() function

# Define the test cases
test_that("convert_config_args converts arguments correctly", {
  # Create a sample data frame in the global environment
  sample_data <- data.frame(a = 1:5, b = 6:10)
  assign("sample_data", sample_data, envir = .GlobalEnv)

  # Define a list of arguments
  args <- list(data = "sample_data", date_var = "a", output_path = "sample_data")

  # Call the function
  result <- convert_config_args(args)

  # Check that data was converted correctly
  expect_true(is.data.frame(result$data))
  expect_equal(result$data, sample_data)

  # Check that output_path was converted correctly
  expect_true(is.data.frame(result$output_path))
  expect_equal(result$output_path, sample_data)
})

test_that("convert_config_args handles null arguments correctly", {
  # Define a list of arguments with NULL values
  args <- list(data = NULL, date_var = NULL, output_path = NULL)

  # Call the function
  result <- convert_config_args(args)

  # Check that the result is the same as the input
  expect_equal(result, args)
})

test_that("convert_config_args handles non-character data arguments correctly", {
  # Define a list of arguments with a data frame directly
  args <- list(data = sample_data, date_var = "a", output_path = "sample_data")

  # Call the function
  result <- convert_config_args(args)

  # Check that data was not changed
  expect_true(is.data.frame(result$data))
  expect_equal(result$data, sample_data)
})

test_that("convert_config_args handles non-character date_var and output_path arguments correctly", {
  # Define a list of arguments with symbols and data frames directly
  args <- list(data = "sample_data", date_var = sym("a"), output_path = sample_data)

  # Call the function
  result <- convert_config_args(args)

  # Check that output_path was not changed
  expect_true(is.data.frame(result$output_path))
  expect_equal(result$output_path, sample_data)
})
