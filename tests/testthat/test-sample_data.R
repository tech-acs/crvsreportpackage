library(testthat)
library(crvsreportpackage)

test_that("Sample Birth data can be read correctly", {
  data <- read_sample_birth_data()
  expect_true(nrow(data) > 0)
  expect_true("birth1a" %in% colnames(data))
})

test_that("Sample Death data can be read correctly", {
  data <- read_sample_death_data()
  expect_true(nrow(data) > 0)
  expect_true("death1a" %in% colnames(data))
})

test_that("Sample Divorce data can be read correctly", {
  data <- read_sample_divorce_data()
  expect_true(nrow(data) > 0)
  expect_true("divorce1a" %in% colnames(data))
})

test_that("Sample Marriage data can be read correctly", {
  data <- read_sample_marriage_data()
  expect_true(nrow(data) > 0)
  expect_true("marriage1a" %in% colnames(data))
})
