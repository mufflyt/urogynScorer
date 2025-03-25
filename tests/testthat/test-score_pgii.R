library(testthat)
library(dplyr)

test_that("score_pgii correctly computes PGI-I scores and classifications", {

  patient_data <- data.frame(
    Record_ID = c("P001", "P002", "P003", "P004", "P005"),
    pgii_response = c(1, 3, 5, NA, 2)
  )

  result <- score_pgii(patient_data, item_name = "pgii_response", id_column = "Record_ID")

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 3)  # ID, score, improvement

  expect_equal(result$pgii_score, c(1, 3, 5, NA, 2))
  expect_equal(result$pgii_improved, c(1, 1, 0, NA, 1))  # (1-3 = improved, 4-7 = not improved)
})

test_that("score_pgii correctly handles missing data", {

  patient_data <- data.frame(
    Record_ID = c("P001", "P002", "P003"),
    pgii_response = c(NA, NA, NA)
  )

  result <- score_pgii(patient_data, item_name = "pgii_response", id_column = "Record_ID")

  expect_true(all(is.na(result$pgii_score)))
  expect_true(all(is.na(result$pgii_improved)))
})

test_that("score_pgii correctly returns the number of valid responses when keep_n_valid = TRUE", {

  patient_data <- data.frame(
    Record_ID = c("P001", "P002", "P003", "P004"),
    pgii_response = c(1, 2, 4, NA)
  )

  result <- score_pgii(patient_data, item_name = "pgii_response", id_column = "Record_ID", keep_n_valid = TRUE)

  expect_equal(ncol(result), 4)  # ID, score, improvement, valid response count
  expect_equal(result$pgii_n_valid[1], 3)  # Three valid responses in total
})

test_that("score_pgii correctly saves results when output_file is specified", {

  patient_data <- data.frame(
    Record_ID = c("P001", "P002"),
    pgii_response = c(1, 5)
  )

  temp_file <- tempfile(fileext = ".csv")

  result <- score_pgii(patient_data, item_name = "pgii_response", id_column = "Record_ID", output_file = temp_file)

  expect_true(file.exists(temp_file))

  saved_data <- read.csv(temp_file)
  expect_equal(saved_data$pgii_score, c(1, 5))
  expect_equal(saved_data$pgii_improved, c(1, 0))  # 1 = improved, 5 = not improved
})

### ✅ **New Tests for Edge Cases and Robustness**

test_that("score_pgii correctly handles all 7 possible PGI-I responses", {

  patient_data <- data.frame(
    Record_ID = paste0("P", 1:7),
    pgii_response = 1:7  # Covering full range from 1 to 7
  )

  result <- score_pgii(patient_data, item_name = "pgii_response", id_column = "Record_ID")

  expect_equal(result$pgii_score, 1:7)
  expect_equal(result$pgii_improved, c(1, 1, 1, 0, 0, 0, 0))  # 1-3 = improved, 4-7 = no improvement
})

test_that("score_pgii correctly handles cases where all patients report no improvement", {

  patient_data <- data.frame(
    Record_ID = c("P001", "P002", "P003"),
    pgii_response = c(4, 6, 7)  # All patients report worsening/no change
  )

  result <- score_pgii(patient_data, item_name = "pgii_response", id_column = "Record_ID")

  expect_equal(result$pgii_improved, c(0, 0, 0))  # No one improved
})

test_that("score_pgii correctly handles cases where all patients improve", {

  patient_data <- data.frame(
    Record_ID = c("P001", "P002", "P003"),
    pgii_response = c(1, 2, 3)  # All patients report improvement
  )

  result <- score_pgii(patient_data, item_name = "pgii_response", id_column = "Record_ID")

  expect_equal(result$pgii_improved, c(1, 1, 1))  # All improved
})

### ❌ **Tests for Error Handling and Input Validation**

test_that("score_pgii throws an error for invalid PGI-I response values", {

  patient_data <- data.frame(
    Record_ID = c("P001", "P002", "P003"),
    pgii_response = c(1, 3, 9)  # 9 is invalid
  )

  expect_error(score_pgii(patient_data, item_name = "pgii_response", id_column = "Record_ID"),
               "PGI-I responses must be integers between 1 and 7 or NA")
})

test_that("score_pgii throws an error when id_column is missing", {

  patient_data <- data.frame(
    PatientID = c("P001", "P002"),
    pgii_response = c(1, 3)
  )

  expect_error(score_pgii(patient_data, item_name = "pgii_response", id_column = "Record_ID"),
               "id_column 'Record_ID' not found in patient_data")
})

test_that("score_pgii throws an error when item_name column is missing", {

  patient_data <- data.frame(
    Record_ID = c("P001", "P002"),
    OtherColumn = c(1, 3)
  )

  expect_error(score_pgii(patient_data, item_name = "pgii_response", id_column = "Record_ID"),
               "item_name 'pgii_response' not found in patient_data")
})

test_that("score_pgii works when id_column is not named Record_ID", {

  patient_data <- data.frame(
    UniqueID = c("A1", "B2", "C3"),
    pgii_response = c(2, 4, 6)
  )

  result <- score_pgii(patient_data, item_name = "pgii_response", id_column = "UniqueID")

  expect_equal(colnames(result), c("UniqueID", "pgii_score", "pgii_improved"))
  expect_equal(result$pgii_improved, c(1, 0, 0))  # First patient improved, others didn't
})

test_that("score_pgii works with large datasets", {

  set.seed(42)
  large_data <- data.frame(
    Record_ID = paste0("P", 1:1000),
    pgii_response = sample(1:7, 1000, replace = TRUE)
  )

  result <- score_pgii(large_data, item_name = "pgii_response", id_column = "Record_ID")

  expect_true(nrow(result) == 1000)
  expect_true(all(result$pgii_score %in% 1:7))
})

test_that("score_pgii logs warning when deprecated keepNvalid parameter is used", {

  patient_data <- data.frame(
    Record_ID = c("P001", "P002"),
    pgii_response = c(1, 3)
  )

  expect_warning(score_pgii(patient_data, item_name = "pgii_response", id_column = "Record_ID", keepNvalid = TRUE),
                 "Parameter 'keepNvalid' is deprecated. Please use 'keep_n_valid' instead.")
})
