# Load the testthat library
library(dplyr)

# Define the tests
test_that("construct_empty_var adds a column with NA values", {
  df <- data.frame(id = 1:5, name = c("Alice", "Bob", "Charlie", "David", "Eve"))
  
  result_df <- construct_empty_var(df)
  expect_true("birth1j" %in% colnames(result_df))
  expect_true(all(is.na(result_df$birth1j)))
})

test_that("construct_empty_var works with a specified column name", {
  df <- data.frame(id = 1:5, name = c("Alice", "Bob", "Charlie", "David", "Eve"))
  
  result_df <- construct_empty_var(df, column_name = "new_col")
  expect_true("new_col" %in% colnames(result_df))
  expect_true(all(is.na(result_df$new_col)))
})

test_that("construct_empty_var does not modify other columns", {
  df <- data.frame(id = 1:5, name = c("Alice", "Bob", "Charlie", "David", "Eve"))
  
  result_df <- construct_empty_var(df)
  expect_equal(result_df$id, df$id)
  expect_equal(result_df$name, df$name)
})