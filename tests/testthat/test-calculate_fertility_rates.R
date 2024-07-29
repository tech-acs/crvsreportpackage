# Unit tests for calculate_fertility_rates
test_that("calculate_fertility_rates calculates age-specific and total fertility rates", {
  # Mock data for testing
  bth_data <- data.frame(
    dobyr = c(2018, 2018, 2019, 2019, 2019, 2020, 2019),
    birth1j = c(NA, NA, NA, NA, NA, NA, 1),
    fert_age_grp = c("15-19", "20-24", "15-19", "20-24", "25-29", "20-24", "20-24")
  )

  population <- data.frame(
    fert_age_grp = c("15-19", "20-24", "25-29"),
    birth2a = c("female", "female", "female"),
    population_2018 = c(10000, 15000, 12000),
    population_2019 = c(10000, 15000, 12000),
    population_2020 = c(10000, 15000, 12000)
  )

  # Dummy implementation of generate_year_sequence to ensure compatibility with the tests
  generate_year_sequence <- function(curr_year) {
    return(curr_year - 2:0)
  }

  result <- calculate_fertility_rates(bth_data, population)

  # Test the structure of the output
  expect_true("fert_age_grp" %in% colnames(result))
  expect_true(any(grepl("^20", colnames(result))))

  # Test that the result contains the correct number of rows
  expect_equal(nrow(result), 4) # 3 age groups + 1 total

  # Test that fertility rates are calculated correctly for a specific age group and year
  # Example for age group 15-19 in 2018
  expect_equal(result[result$fert_age_grp == "15-19", "2018"], 0.1) # 1 birth / 10000 * 1000
})
