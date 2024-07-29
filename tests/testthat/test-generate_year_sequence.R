library(testthat)

test_that("generate_year_sequence generates correct sequence of years", {
  result <- generate_year_sequence(2023, 5)
  expected <- c(2019, 2020, 2021, 2022, 2023)
  expect_equal(result, expected)
})

test_that("generate_year_sequence handles default num_yrs correctly", {
  result <- generate_year_sequence(2023)
  expected <- c(2019, 2020, 2021, 2022, 2023)
  expect_equal(result, expected)
})

test_that("generate_year_sequence returns a single year sequence if num_yrs is 1", {
  result <- generate_year_sequence(2023, 1)
  expected <- c(2023)
  expect_equal(result, expected)
})

test_that("generate_year_sequence handles non-numeric latest_year", {
  expect_error(generate_year_sequence("2023", 5), "Both latest_year and num_yrs must be numeric")
})

test_that("generate_year_sequence handles non-numeric num_yrs", {
  expect_error(generate_year_sequence(2023, "5"), "Both latest_year and num_yrs must be numeric")
})

test_that("generate_year_sequence handles non-positive latest_year", {
  expect_error(generate_year_sequence(-2023, 5), "Both latest_year and num_yrs must be positive")
})

test_that("generate_year_sequence handles non-positive num_yrs", {
  expect_error(generate_year_sequence(2023, -5), "Both latest_year and num_yrs must be positive")
})

test_that("generate_year_sequence handles unrealistic latest_year", {
  expect_error(generate_year_sequence(1940, 5), "latest_year must be a realistic year value between 1950 and the current year")
  expect_error(generate_year_sequence(2050, 5), "latest_year must be a realistic year value between 1950 and the current year")
})

test_that("generate_year_sequence handles edge case of current year", {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  result <- generate_year_sequence(current_year, 5)
  expected <- seq(current_year - 4, current_year)
  expect_equal(result, expected)
})

test_that("generate_year_sequence handles edge case of earliest realistic year", {
  result <- generate_year_sequence(1950, 5)
  expected <- c(1946, 1947, 1948, 1949, 1950)
  expect_equal(result, expected)
})
