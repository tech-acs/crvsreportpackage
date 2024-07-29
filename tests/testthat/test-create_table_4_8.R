test_that("create_t4.8 function works correctly", {
  working_dir <- getwd()
  setwd("../../")

  # Sample data
  bth_data <- data.frame(
    dobyr = c(2022, 2022, 2022, 2022, 2022),
    birth1c = c("Region_A", "Region_A", "Region_B", "Region_B", "Region_A")
  )

  bth_est <- data.frame(
    year = c(2022, 2022),
    birth1c = c("Region_A", "Region_B"),
    total = c(300, 200)
  )

  pops <- data.frame(
    birth1c = c("Region_A", "Region_B"),
    population_2022 = c(1000, 800)
  )

  # Call the function
  result <- create_t4.8(bth_data, bth_est, pops, date_var = "dobyr", data_year = 2022, by_var = "birth1c", tablename = "Table_4_8", output_path = "outputs/")

  # Check if result is a data frame
  expect_s3_class(result, "data.frame")

  # Check for expected columns in the result
  expected_cols <- c("birth1c", "total", "adjusted", "cbr")
  expect_equal(colnames(result), expected_cols)

  # Check if the file is written
  expect_true(file.exists("outputs/Table_4_8.csv"))

  # Clean up the generated file after the test
  unlink("outputs/Table_4_8.csv")
  # Reset working dir
  setwd(working_dir)

})
