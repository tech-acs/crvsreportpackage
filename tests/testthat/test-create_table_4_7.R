test_that("create_t4.7 function works correctly", {
  working_dir <- getwd()
  setwd("../../")

  # Sample data
  bth_data <- data.frame(
    dobyr = c(2022, 2022, 2022, 2022, 2022, 2022),
    birth1j = c(NA, NA, NA, NA, NA, NA),
    birth1c = c("Region_A", "Region_A", "Region_B", "Region_B", "Region_A", "Region_A"),
    birth1i = c("Hospital", "Home", "Hospital", "Home", "Hospital", "Home"),
    birth1h = c("Physician", "Nurse", "Midwife", "Other", "Nurse", "Not stated")
  )

  # Call the function
  result <- create_t4.7(bth_data, date_var = "dobyr", data_year = 2022, tablename = "Table_4_7", output_path = "outputs/")

  # Check if result is a data frame
  expect_s3_class(result, "data.frame")

  # Check if the file is written
  expect_true(file.exists("outputs/Table_4_7.csv"))

  # Clean up the generated file after the test
  unlink("outputs/Table_4_7.csv")
  # Reset working dir
  setwd(working_dir)
})
