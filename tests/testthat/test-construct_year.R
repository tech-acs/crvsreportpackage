# Define the unit tests
test_that("construct_year works correctly with default date column", {
  df <- data.frame(birth1c = c("1990-05-15", "1985-10-30", "2000-07-22"))
  expected_df <- data.frame(
    birth1c = as.Date(c("1990-05-15", "1985-10-30", "2000-07-22")),
    dobyr = c(1990, 1985, 2000)
  )

  result_df <- construct_year(df)

  expect_equal(result_df, expected_df)
})

test_that("construct_year works correctly with specified date column", {
  df <- data.frame(event_date = c("1990-05-15", "1985-10-30", "2000-07-22"))
  expected_df <- data.frame(
    event_date = as.Date(c("1990-05-15", "1985-10-30", "2000-07-22")),
    dobyr = c(1990, 1985, 2000)
  )

  result_df <- construct_year(df, date_col = "event_date")

  expect_equal(result_df, expected_df)
})

test_that("construct_year works correctly with specified year column", {
  df <- data.frame(dor = c("1990-05-15", "1985-10-30", "2000-07-22"))
  expected_df <- data.frame(
    dor = as.Date(c("1990-05-15", "1985-10-30", "2000-07-22")),
    doryr = c(1990, 1985, 2000)
  )

  result_df <- construct_year(df, date_col = "dor", year_col = "doryr")

  expect_equal(result_df, expected_df)
})
