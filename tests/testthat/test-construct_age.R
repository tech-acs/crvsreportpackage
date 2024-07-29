# Load necessary library
library(dplyr)

# Define the tests
test_that("construct_age works correctly with default age column and today's date", {
  df <- data.frame(dob = as.Date(c("1990-05-15", "1985-10-30", "2000-07-22")))

  # Calculate expected ages
  today <- Sys.Date()
  expected_ages <- as.integer((today - df$dob) / 365.25)
  expected_df <- df %>%
    mutate(birth3b = expected_ages)

  result_df <- construct_age(df, dob_col = "dob")

  expect_equal(result_df, expected_df)
})

test_that("construct_age works correctly with specified age column and end date column", {
  df <- data.frame(death2a = as.Date(c("1990-05-15", "1985-10-30", "2000-07-22")),
                   death1a = as.Date(c("2022-07-15", "2022-07-15", "2022-07-15")))

  # Calculate expected ages
  expected_ages <- as.integer((df$death1a - df$death2a) / 365.25)
  expected_df <- df %>%
    mutate(death2b = expected_ages)

  result_df <- construct_age(df, dob_col = "death2a", age_col = "death2b", end_date_col = "death1a")

  expect_equal(result_df, expected_df)
})

test_that("construct_age works correctly with default age column and specified end date column", {
  df <- data.frame(dob = as.Date(c("1990-05-15", "1985-10-30", "2000-07-22")),
                   end_date = as.Date(c("2022-07-15", "2022-07-15", "2022-07-15")))

  # Calculate expected ages
  expected_ages <- as.integer((df$end_date - df$dob) / 365.25)
  expected_df <- df %>%
    mutate(birth3b = expected_ages)

  result_df <- construct_age(df, dob_col = "dob", end_date_col = "end_date")

  expect_equal(result_df, expected_df)
})

test_that("construct_age handles missing end date column by using today's date", {
  df <- data.frame(dob = as.Date(c("1990-05-15", "1985-10-30", "2000-07-22")))

  # Calculate expected ages
  today <- Sys.Date()
  expected_ages <- as.integer((today - df$dob) / 365.25)
  expected_df <- df %>%
    mutate(birth3b = expected_ages)

  result_df <- construct_age(df, dob_col = "dob")

  expect_equal(result_df, expected_df)
})

test_that("construct_age handles different date formats", {
  df <- data.frame(dob = c("1990/05/15", "1985-10-30", "2000.07.22"))
  df$dob <- as.Date(df$dob, format = "%Y/%m/%d")

  # Calculate expected ages
  today <- Sys.Date()
  expected_ages <- as.integer((today - df$dob) / 365.25)
  expected_df <- df %>%
    mutate(birth3b = expected_ages)

  result_df <- construct_age(df, dob_col = "dob")

  expect_equal(result_df, expected_df)
})
