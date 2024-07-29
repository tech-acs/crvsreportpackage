# Load the testthat library
library(dplyr)

# Define the tests
test_that("construct_age_group adds a column with correct age groups (default group size)", {
  df <- data.frame(id = 1:10, age = c(23, 45, 34, 56, 29, 12, 67, 44, 33, 55))
  result_df <- construct_age_group(df, age_col = "age")

  expect_true("fert_age_grp" %in% colnames(result_df))
  expected_groups <- c("20-24", "45-49", "30-34", "55-59", "25-29", "10-14", "65-69", "40-44", "30-34", "55-59")
  expect_equal(as.character(result_df$fert_age_grp), expected_groups)
})

test_that("construct_age_group adds a column with correct age groups (specified group size)", {
  df <- data.frame(id = 1:10, age = c(23, 45, 34, 56, 29, 12, 67, 44, 33, 55))
  result_df <- construct_age_group(df, age_col = "age", group_size = 10)

  expect_true("fert_age_grp" %in% colnames(result_df))
  expected_groups <- c("20-29", "40-49", "30-39", "50-59", "20-29", "10-19", "60-69", "40-49", "30-39", "50-59")
  expect_equal(as.character(result_df$fert_age_grp), expected_groups)
})

test_that("construct_age_group works with different column name for age groups", {
  df <- data.frame(id = 1:10, age = c(23, 45, 34, 56, 29, 12, 67, 44, 33, 55))
  result_df <- construct_age_group(df, age_col = "age", new_col = "age_group")

  expect_true("age_group" %in% colnames(result_df))
  expected_groups <- c("20-24", "45-49", "30-34", "55-59", "25-29", "10-14", "65-69", "40-44", "30-34", "55-59")
  expect_equal(as.character(result_df$age_group), expected_groups)
})

test_that("construct_age_group handles NA values in age column", {
  df <- data.frame(id = 1:5, age = c(23, NA, 34, 56, 29))
  result_df <- construct_age_group(df, age_col = "age")

  expect_true("fert_age_grp" %in% colnames(result_df))
  expected_groups <- c("20-24", NA, "30-34", "55-59", "25-29")
  expect_equal(as.character(result_df$fert_age_grp), expected_groups)
})
