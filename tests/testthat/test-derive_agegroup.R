library(testthat)
library(dplyr)

# Unit Tests
test_that("derive_age_groups creates correct age groups with under_1 set to TRUE", {
  data <- data.frame(age = c(0, 0.5, 1, 3, 5, 10, 25, 30, 35, 40, 50, 85, 90, 95, 100))
  result <- derive_age_groups(data$age, start_age = 5, max_band = 95, step_size = 5, under_1 = TRUE)

  expected <- c("Under 1", "Under 1", "01-04", "01-04", "05-09", "10-14", "25-29", "30-34", "35-39", "40-44", "50-54", "85-89", "90-94", "95 and over", "95 and over")

  expect_equal(result, expected)
})

test_that("derive_age_groups creates correct age groups with under_1 set to FALSE", {
  data <- data.frame(age = c(0, 0.5, 1, 3, 5, 10, 25, 30, 35, 40, 50, 85, 90, 95, 100))
  result <- derive_age_groups(data$age, start_age = 5, max_band = 95, step_size = 5, under_1 = FALSE)

  expected <- c("Under 5", "Under 5", "Under 5", "Under 5", "05-09", "10-14", "25-29", "30-34", "35-39", "40-44", "50-54", "85-89", "90-94", "95 and over", "95 and over")

  expect_equal(result, expected)
})

test_that("derive_age_groups handles edge cases correctly", {
  data <- data.frame(age = c(-1, 1, 5, 10, 50, 100))
  result <- derive_age_groups(data$age, start_age = 5, max_band = 95, step_size = 10, under_1 = TRUE)

  expected <- c(NA, "01-04", "05-14", "05-14", "45-54", "95 and over")

  expect_equal(result, expected)
})

test_that("derive_age_groups handles different step sizes", {
  data <- data.frame(age = c(1, 5, 10, 15, 20, 25))
  result <- derive_age_groups(data$age, start_age = 5, max_band = 25, step_size = 10, under_1 = FALSE)

  expected <- c("Under 5", "05-14", "05-14", "15-24", "15-24", "25 and over")

  expect_equal(result, expected)
})
