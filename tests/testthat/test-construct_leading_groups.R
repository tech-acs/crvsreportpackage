# Load additional package
library(dplyr)

test_that("construct_leading_groups function works correctly", {
  # Sample data
  data <- data.frame(
    death2b = c(3, 8, 45, 72, -1, 100)
  )

  # Run the function
  result <- construct_leading_groups(data, age_col = "death2b", age_group_col = "age_grp_lead")

  # Expected result
  expected_result <- data.frame(
    death2b = c(3, 8, 45, 72, -1, 100),
    age_grp_lead = c("0-4", "5-14", "15-69", "70+", NA, "70+")
  )

  # Check if the result is as expected
  expect_equal(result, expected_result)

  # Additional tests with different column names
  data <- data.frame(
    years = c(3, 8, 45, 72, 1, 65, 70),
    name = c("A", "B", "C", "D", "E", "F", "G")
  )

  result <- construct_leading_groups(data, age_col = "years", age_group_col = "age_group")

  expected_result <- data.frame(
    years = c(3, 8, 45, 72, 1, 65, 70),
    name = c("A", "B", "C", "D", "E", "F", "G"),
    age_group = c("0-4", "5-14", "15-69", "70+", "0-4", "15-69", "70+")
  )

  expect_equal(result, expected_result)
})
