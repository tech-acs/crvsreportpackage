test_that("create_t4.3 function works correctly", {
  working_dir <- getwd()
  setwd("../../")

  # Sample data
  bth_data <- data.frame(
    dobyr = c(2022, 2022, 2022, 2022, 2022),
    birth1j = c(NA, NA, NA, NA, NA),
    birth1c = c("Region_A", "Region_A", "Region_C", "Region_B", "Region_D"),
    birth3l = c("Region_A", "Region_B", "Region_B", "Region_D", "Region_C")
  )

  # Call the function
  result <- create_t4.3(bth_data, date_var = "dobyr", data_year = 2022, tablename = "Table_4_3", output_path = "outputs/")

  # Check if result is a data frame
  expect_s3_class(result, "data.frame")

  # Check if the file is written
  expect_true(file.exists("outputs/Table_4_3.csv"))

  # Check for expected columns in the result
  expected_cols <- c("birth1c", "Other location", "Same as place of occurrence", "Total")
  expect_equal(colnames(result), expected_cols)

  # Clean up the generated file after the test
  unlink("outputs/Table_4_3.csv")

  # Reset working dir
  setwd(working_dir)
})
