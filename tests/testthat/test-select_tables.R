library(testthat)

# Unit Tests
test_that("select_tables returns correct table IDs for a given section number", {
  all_tables <- list(
    list(table_id = "3_1", table_name = "Summary Table", function_name = "create_t3.1"),
    list(table_id = "3_2", table_name = "Detailed Table", function_name = "create_t3.2_t3.3"),
    list(table_id = "4_1", table_name = "Summary Table", function_name = "create_t4.1"),
    list(table_id = "4_2", table_name = "Detailed Table", function_name = "create_t4.2"),
    list(table_id = "5_1", table_name = "Summary Table", function_name = "create_t5.1")
  )

  result <- select_tables("3", all_tables)
  expected <- list("3_1", "3_2")
  expect_equal(result, expected)
})

test_that("select_tables returns empty list when no table IDs match the section number", {
  all_tables <- list(
    list(table_id = "3_1", table_name = "Summary Table", function_name = "create_t3.1"),
    list(table_id = "3_2", table_name = "Detailed Table", function_name = "create_t3.2_t3.3"),
    list(table_id = "4_1", table_name = "Summary Table", function_name = "create_t4.1"),
    list(table_id = "4_2", table_name = "Detailed Table", function_name = "create_t4.2"),
    list(table_id = "5_1", table_name = "Summary Table", function_name = "create_t5.1")
  )

  result <- select_tables("6", all_tables)
  expected <- list()
  expect_equal(result, expected)
})
