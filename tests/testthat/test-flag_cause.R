library(testthat)
library(dplyr)

test_that("flag_cause correctly flags rows based on exact match", {
  data <- data.frame(
    ID = 1:5,
    FIC10MEN1 = c("E85", "E619", "E85", "A123", "B456"),
    FIC10MEN2 = c("E619", "A123", "B456", "E85", "E85")
  )

  result <- flag_cause(data, "E85|E619", "FIC10MEN", "CHECK")

  expected <- c(1, 1, 1, 1, 1)

  expect_equal(result$CHECK, expected)
})

test_that("flag_cause correctly flags rows based on partial match", {
  data <- data.frame(
    ID = 1:5,
    FIC10MEN1 = c("E851", "E619A", "E852", "A123", "B456"),
    FIC10MEN2 = c("E619B", "A123", "B456", "E853", "E854")
  )

  result <- flag_cause(data, "E85", "FIC10MEN", "CHECK")

  expected <- c(1, 0, 1, 1, 1)

  expect_equal(result$CHECK, expected)
})

test_that("flag_cause creates new column with 0s if no match found", {
  data <- data.frame(
    ID = 1:5,
    FIC10MEN1 = c("A123", "B456", "C789", "D012", "E345"),
    FIC10MEN2 = c("F678", "G910", "H112", "I314", "J516")
  )

  result <- flag_cause(data, "E85", "FIC10MEN", "CHECK")

  expected <- c(0, 0, 0, 0, 0)

  expect_equal(result$CHECK, expected)
})

test_that("flag_cause handles multiple code patterns", {
  data <- data.frame(
    ID = 1:5,
    FIC10MEN1 = c("E851", "E619", "E85", "A123", "B456"),
    FIC10MEN2 = c("E619", "A123", "B456", "E85", "E619")
  )

  result <- flag_cause(data, "E85|E619", "FIC10MEN", "CHECK")

  expected <- c(1, 1, 1, 1, 1)

  expect_equal(result$CHECK, expected)
})

test_that("flag_cause works with no matching columns", {
  data <- data.frame(
    ID = 1:5,
    DIFFCOL1 = c("E85", "E619", "E85", "A123", "B456"),
    DIFFCOL2 = c("E619", "A123", "B456", "E85", "E85")
  )

  result <- flag_cause(data, "E85|E619", "FIC10MEN", "CHECK")

  expected <- rep(0, 5)  # No columns matched, hence all 0s

  expect_equal(result$CHECK, expected)
})

test_that("flag_cause works with empty dataset", {
  data <- data.frame(
    ID = integer(0),
    FIC10MEN1 = character(0),
    FIC10MEN2 = character(0)
  )

  result <- flag_cause(data, "E85|E619", "FIC10MEN", "CHECK")

  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 4)  # Original columns + new column
})
