library(testthat)
library(dplyr)
test_that("score_pgii works with standard inputs", {
  # Create sample test data
  test_data <- tibble::tibble(
    Record_ID = c("P001", "P002", "P003", "P004", "P005"),
    pgii_response = c(1, 2, 3, 4, NA)
  )

  # Score the data
  scores <- score_pgii(test_data, item_name = "pgii_response")

  # Test that output has the right structure
  expect_true(is.data.frame(scores))
  expect_equal(nrow(scores), 5)
  expect_equal(colnames(scores), c("Record_ID", "pgii_score", "pgii_improved"))

  # Test that the scores are calculated correctly
  expect_equal(scores$pgii_score, c(1, 2, 3, 4, NA))
  expect_equal(scores$pgii_improved, c(1, 1, 1, 0, NA))
})

test_that("score_pgii works with custom ID column", {
  # Create sample test data with custom ID column
  test_data <- tibble::tibble(
    Patient_ID = c("PT1", "PT2", "PT3"),
    pgii_response = c(5, 6, 7)
  )

  # Score the data with custom ID column
  scores <- score_pgii(test_data,
                       item_name = "pgii_response",
                       id_column = "Patient_ID")

  # Test that the output has the right structure
  expect_true(is.data.frame(scores))
  expect_equal(nrow(scores), 3)
  expect_equal(colnames(scores), c("Patient_ID", "pgii_score", "pgii_improved"))

  # Test that the scores are calculated correctly
  expect_equal(scores$pgii_score, c(5, 6, 7))
  expect_equal(scores$pgii_improved, c(0, 0, 0))
})

test_that("score_pgii works with keep_n_valid = TRUE", {
  # Create sample test data
  test_data <- tibble::tibble(
    Record_ID = c("P001", "P002", "P003", "P004", "P005"),
    pgii_response = c(1, 2, 3, 4, NA)
  )

  # Score the data with keep_n_valid = TRUE
  scores <- score_pgii(test_data,
                       item_name = "pgii_response",
                       keep_n_valid = TRUE)

  # Test that the output has the right structure
  expect_true(is.data.frame(scores))
  expect_equal(nrow(scores), 5)
  expect_equal(colnames(scores), c("Record_ID", "pgii_score", "pgii_improved", "pgii_n_valid"))

  # Test that the scores are calculated correctly
  expect_equal(scores$pgii_score, c(1, 2, 3, 4, NA))
  expect_equal(scores$pgii_improved, c(1, 1, 1, 0, NA))
  expect_equal(scores$pgii_n_valid, rep(4, 5))
})

test_that("score_pgii correctly handles output_file", {
  # Create sample test data
  test_data <- tibble::tibble(
    Record_ID = c("P001", "P002", "P003"),
    pgii_response = c(1, 3, 5)
  )

  # Create a temporary file for testing
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file), add = TRUE)

  # Score the data and save to temp file
  score_pgii(test_data,
             item_name = "pgii_response",
             output_file = temp_file)

  # Check that the file exists and can be read
  expect_true(file.exists(temp_file))
  file_content <- utils::read.csv(temp_file)

  # Test that the file content is correct
  expect_equal(nrow(file_content), 3)
  expect_equal(colnames(file_content), c("Record_ID", "pgii_score", "pgii_improved"))
  expect_equal(file_content$pgii_score, c(1, 3, 5))
  expect_equal(file_content$pgii_improved, c(1, 1, 0))
})

test_that("score_pgii handles invalid inputs correctly", {
  # Create sample test data with invalid responses
  test_data <- tibble::tibble(
    Record_ID = c("P001", "P002", "P003"),
    pgii_response = c(1, 8, 0)  # Valid is 1-7
  )

  # Expect error for invalid responses
  expect_error(
    score_pgii(test_data, item_name = "pgii_response"),
    "PGI-I responses must be integers between 1 and 7 or NA"
  )

  # Test with non-numeric responses
  test_data$pgii_response <- c(1, "a", 3)
  expect_error(
    score_pgii(test_data, item_name = "pgii_response"),
    "PGI-I responses must be integers between 1 and 7 or NA"
  )
})


test_that("score_pgii works with verbose = TRUE", {
  # Create sample test data
  test_data <- tibble::tibble(
    Record_ID = c("P001", "P002"),
    pgii_response = c(1, 4)
  )

  # Expect no error with verbose = TRUE
  expect_no_error(
    score_pgii(test_data, item_name = "pgii_response", verbose = TRUE)
  )
})

