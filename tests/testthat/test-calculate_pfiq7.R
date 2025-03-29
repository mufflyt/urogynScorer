# Test file: tests/testthat/test-calculate_pfiq7.R

test_that("calculate_pfiq7 works with standard inputs", {
  # Create sample test data
  test_data <- data.frame(
    uiq1 = c(1, 2, 0), uiq2 = c(2, 1, 1), uiq3 = c(0, 0, 0),
    uiq4 = c(1, 2, 1), uiq5 = c(3, 2, 0), uiq6 = c(0, 1, 0),
    uiq7 = c(2, 2, 1),
    craiq1 = c(0, 1, 0), craiq2 = c(1, 0, 0), craiq3 = c(0, 0, 0),
    craiq4 = c(0, 1, 0), craiq5 = c(0, 0, 0), craiq6 = c(1, 0, 0),
    craiq7 = c(0, 0, 0),
    popiq1 = c(2, 3, 0), popiq2 = c(1, 2, 0), popiq3 = c(2, 1, 0),
    popiq4 = c(0, 0, 0), popiq5 = c(1, 0, 0), popiq6 = c(2, 1, 0),
    popiq7 = c(3, 2, 0)
  )

  # Calculate scores
  pfiq7_scores <- calculate_pfiq7(test_data)

  # Test structure of output
  expect_s3_class(pfiq7_scores, "data.frame")
  expect_equal(nrow(pfiq7_scores), 3)

  # Based on the actual implementation
  expect_equal(colnames(pfiq7_scores),
               c("patient_id", "uiq7_score", "craiq7_score", "popiq7_score", "pfiq7_total"))

  # Test scores for first patient
  # The actual implementation calculates mean score for each domain, then multiplies by 100/21
  expect_equal(pfiq7_scores$uiq7_score[1], 42.9, tolerance = 0.1)
  expect_equal(pfiq7_scores$craiq7_score[1], 9.5, tolerance = 0.1)
  expect_equal(pfiq7_scores$popiq7_score[1], 52.4, tolerance = 0.1)
  expect_equal(pfiq7_scores$pfiq7_total[1], 104.8, tolerance = 0.1)
})

test_that("calculate_pfiq7 handles missing values correctly", {
  # Create test data with missing values
  test_data <- data.frame(
    uiq1 = c(1, 2, NA), uiq2 = c(2, 1, 1), uiq3 = c(0, 0, 0),
    uiq4 = c(1, NA, 1), uiq5 = c(3, 2, 0), uiq6 = c(0, 1, 0),
    uiq7 = c(2, 2, 1),
    craiq1 = c(0, 1, 0), craiq2 = c(NA, 0, 0), craiq3 = c(0, 0, 0),
    craiq4 = c(0, 1, NA), craiq5 = c(0, 0, 0), craiq6 = c(1, 0, 0),
    craiq7 = c(0, 0, 0),
    popiq1 = c(2, 3, 0), popiq2 = c(1, NA, 0), popiq3 = c(2, 1, 0),
    popiq4 = c(0, 0, 0), popiq5 = c(1, 0, NA), popiq6 = c(2, 1, 0),
    popiq7 = c(NA, 2, 0)
  )

  # Calculate scores
  pfiq7_scores <- calculate_pfiq7(test_data)

  # Test that scores are calculated correctly with missing values
  # Based on the actual implementation behavior
  expect_equal(pfiq7_scores$uiq7_score[1], 42.9, tolerance = 0.1)
  expect_equal(pfiq7_scores$popiq7_score[1], 44.4, tolerance = 0.1)  # Corrected value

  # Second patient has 1 missing value in each domain, check calculations
  expect_false(is.na(pfiq7_scores$uiq7_score[2]))
  expect_false(is.na(pfiq7_scores$craiq7_score[2]))
  expect_false(is.na(pfiq7_scores$popiq7_score[2]))

  # Third patient has missing values in all domains
  expect_false(is.na(pfiq7_scores$uiq7_score[3]))
  expect_false(is.na(pfiq7_scores$craiq7_score[3]))
  expect_false(is.na(pfiq7_scores$popiq7_score[3]))

  # Check that total is not NA if none of the domains are NA
  expect_false(is.na(pfiq7_scores$pfiq7_total[3]))
})


test_that("calculate_pfiq7 handles non-numeric responses", {
  # Create test data with non-numeric responses
  test_data <- data.frame(
    uiq1 = c(1, "2", "0"), uiq2 = c(2, "1", "1"), uiq3 = c(0, "0", "0"),
    uiq4 = c(1, "2", "1"), uiq5 = c(3, "2", "0"), uiq6 = c(0, "1", "0"),
    uiq7 = c(2, "2", "1"),
    craiq1 = c(0, "1", "0"), craiq2 = c(1, "0", "0"), craiq3 = c(0, "0", "0"),
    craiq4 = c(0, "1", "0"), craiq5 = c(0, "0", "0"), craiq6 = c(1, "0", "0"),
    craiq7 = c(0, "0", "0"),
    popiq1 = c(2, "3", "0"), popiq2 = c(1, "2", "0"), popiq3 = c(2, "1", "0"),
    popiq4 = c(0, "0", "0"), popiq5 = c(1, "0", "0"), popiq6 = c(2, "1", "0"),
    popiq7 = c(3, "2", "0"),
    stringsAsFactors = FALSE
  )

  # The function handles string-to-numeric conversion without throwing warnings
  # Just test that it works correctly
  pfiq7_scores <- calculate_pfiq7(test_data, verbose = TRUE)

  # Verify scores match expected values
  first_patient_scores <- pfiq7_scores[1, ]
  expect_equal(first_patient_scores$uiq7_score, 42.9, tolerance = 0.1)
  expect_equal(first_patient_scores$craiq7_score, 9.5, tolerance = 0.1)
  expect_equal(first_patient_scores$popiq7_score, 52.4, tolerance = 0.1)
})


test_that("calculate_pfiq7 handles patient with all missing responses", {
  # Create test data with a patient having all missing responses
  test_data <- data.frame(
    uiq1 = c(1, NA, 0), uiq2 = c(2, NA, 1), uiq3 = c(0, NA, 0),
    uiq4 = c(1, NA, 1), uiq5 = c(3, NA, 0), uiq6 = c(0, NA, 0),
    uiq7 = c(2, NA, 1),
    craiq1 = c(0, NA, 0), craiq2 = c(1, NA, 0), craiq3 = c(0, NA, 0),
    craiq4 = c(0, NA, 0), craiq5 = c(0, NA, 0), craiq6 = c(1, NA, 0),
    craiq7 = c(0, NA, 0),
    popiq1 = c(2, NA, 0), popiq2 = c(1, NA, 0), popiq3 = c(2, NA, 0),
    popiq4 = c(0, NA, 0), popiq5 = c(1, NA, 0), popiq6 = c(2, NA, 0),
    popiq7 = c(3, NA, 0)
  )

  # Calculate scores
  pfiq7_scores <- calculate_pfiq7(test_data)

  # Second patient should have NA for all scores
  expect_true(is.na(pfiq7_scores$uiq7_score[2]))
  expect_true(is.na(pfiq7_scores$craiq7_score[2]))
  expect_true(is.na(pfiq7_scores$popiq7_score[2]))
  expect_true(is.na(pfiq7_scores$pfiq7_total[2]))

  # Other patients should have valid scores
  expect_false(is.na(pfiq7_scores$uiq7_score[1]))
  expect_false(is.na(pfiq7_scores$uiq7_score[3]))
})
