# Test file for PISQ-12 calculation function
# tests/testthat/test-calculate_pisq12.R

# PASSED
testthat::test_that("calculate_pisq12 works with complete data", {

  logger::log_info("Test: Complete data without missing values")

  sample_data <- data.frame(
    id = 1:2,
    pisq_1 = c(2, 3), pisq_2 = c(4, 3), pisq_3 = c(1, 2),
    pisq_4 = c(3, 4), pisq_5 = c(2, 3), pisq_6 = c(4, 2),
    pisq_7 = c(1, 3), pisq_8 = c(2, 3), pisq_9 = c(3, 2),
    pisq_10 = c(4, 3), pisq_11 = c(3, 2), pisq_12 = c(1, 3)
  )

  scores <- calculate_pisq12(sample_data, verbose = TRUE)

  testthat::expect_s3_class(scores, "data.frame")
  testthat::expect_true(all(c("pisq12_score", "pisq12_missing", "pisq12_valid") %in% colnames(scores)))
  testthat::expect_equal(scores$pisq12_missing, c(0, 0))
  testthat::expect_true(all(scores$pisq12_valid))
})

testthat::test_that("calculate_pisq12 handles missing values correctly", {

  logger::log_info("Test: Data with missing values below threshold")

  sample_data <- data.frame(
    id = 1:3,
    pisq_1 = c(2, 3, 1),
    pisq_2 = c(4, 3, 2),
    pisq_3 = c(1, 2, NA),
    pisq_4 = c(3, 4, 2),
    pisq_5 = c(2, 3, 4),
    pisq_6 = c(4, 2, 3),
    pisq_7 = c(1, 3, 4),
    pisq_8 = c(2, 3, 1),
    pisq_9 = c(3, 2, 1),
    pisq_10 = c(4, 3, 2),
    pisq_11 = c(3, 2, NA),
    pisq_12 = c(1, 3, 4)
  )

  scores <- calculate_pisq12(sample_data, verbose = TRUE)

  testthat::expect_equal(scores$pisq12_missing, c(0, 0, 2))
  testthat::expect_true(all(scores$pisq12_valid))
  testthat::expect_false(any(is.na(scores$pisq12_score)))
})

testthat::test_that("calculate_pisq12 invalidates scores with too many missing items", {

  logger::log_info("Test: Data with missing values above threshold")

  sample_data <- data.frame(
    id = c("A", "B", "C"),
    pisq_1 = c(2, NA, 1),
    pisq_2 = c(4, 3, NA),
    pisq_3 = c(1, 2, 3),
    pisq_4 = c(3, NA, 2),
    pisq_5 = c(2, 3, 4),
    pisq_6 = c(4, 2, 3),
    pisq_7 = c(1, NA, 4),
    pisq_8 = c(2, 3, 1),
    pisq_9 = c(3, 2, 1),
    pisq_10 = c(4, 3, NA),
    pisq_11 = c(3, 2, 4),
    pisq_12 = c(1, NA, 4)
  )

  scores <- calculate_pisq12(sample_data, max_missing = 2, verbose = TRUE)

  testthat::expect_equal(scores$pisq12_missing, c(0, 4, 2))
  testthat::expect_equal(scores$pisq12_valid, c(TRUE, FALSE, TRUE))
  testthat::expect_true(is.na(scores$pisq12_score[2])) # Expect NA for invalid score
})

testthat::test_that("calculate_pisq12 handles reverse scoring correctly", {

  logger::log_info("Test: Reverse scoring verification")

  # All 0 responses should be reverse-scored to all 4s for items 1-4
  sample_data <- data.frame(
    id = 1,
    pisq_1 = 0, pisq_2 = 0, pisq_3 = 0, pisq_4 = 0,
    pisq_5 = 4, pisq_6 = 4, pisq_7 = 4, pisq_8 = 4,
    pisq_9 = 4, pisq_10 = 4, pisq_11 = 4, pisq_12 = 4
  )

  scores <- calculate_pisq12(sample_data, verbose = TRUE)

  # Expected total: 16 (from reversed 0s to 4s) + 32 (from 4s) = 48
  testthat::expect_equal(scores$pisq12_score[1], 48)
})

testthat::test_that("calculate_pisq12 works with custom prefix", {

  logger::log_info("Test: Custom column prefix")

  sample_data <- data.frame(
    subject_id = 1:2,
    q_1 = c(2, 3), q_2 = c(4, 3), q_3 = c(1, 2),
    q_4 = c(3, 4), q_5 = c(2, 3), q_6 = c(4, 2),
    q_7 = c(1, 3), q_8 = c(2, 3), q_9 = c(3, 2),
    q_10 = c(4, 3), q_11 = c(3, 2), q_12 = c(1, 3)
  )

  scores <- calculate_pisq12(sample_data, item_prefix = "q_", verbose = TRUE)

  testthat::expect_true("subject_id" %in% colnames(scores))
  testthat::expect_equal(nrow(scores), 2)
  testthat::expect_true(all(c("pisq12_score", "pisq12_missing", "pisq12_valid") %in% colnames(scores)))
})

testthat::test_that("calculate_pisq12 handles non-numeric data correctly", {

  logger::log_info("Test: Non-numeric data conversion")

  sample_data <- data.frame(
    id = 1:2,
    pisq_1 = c("2", "3"), pisq_2 = c("4", "3"), pisq_3 = c("1", "2"),
    pisq_4 = c("3", "4"), pisq_5 = c("2", "3"), pisq_6 = c("4", "2"),
    pisq_7 = c("1", "3"), pisq_8 = c("2", "3"), pisq_9 = c("3", "2"),
    pisq_10 = c("4", "3"), pisq_11 = c("3", "2"), pisq_12 = c("1", "3")
  )

  scores <- calculate_pisq12(sample_data, verbose = TRUE)

  testthat::expect_s3_class(scores, "data.frame")
  testthat::expect_equal(scores$pisq12_missing, c(0, 0))
})

testthat::test_that("calculate_pisq12 handles 1-5 scale data correctly", {

  logger::log_info("Test: 1-5 scale auto-correction")

  sample_data <- data.frame(
    id = 1:2,
    pisq_1 = c(3, 4), pisq_2 = c(5, 4), pisq_3 = c(2, 3),
    pisq_4 = c(4, 5), pisq_5 = c(3, 4), pisq_6 = c(5, 3),
    pisq_7 = c(2, 4), pisq_8 = c(3, 4), pisq_9 = c(4, 3),
    pisq_10 = c(5, 4), pisq_11 = c(4, 3), pisq_12 = c(2, 4)
  )

  # This should auto-correct the 5 values to 4
  scores <- calculate_pisq12(sample_data, verbose = TRUE)

  testthat::expect_s3_class(scores, "data.frame")
  testthat::expect_true(all(scores$pisq12_valid))
})
