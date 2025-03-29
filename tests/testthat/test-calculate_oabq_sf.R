library(testthat)
library(dplyr)

# Create sample test data for use in multiple tests
create_test_data <- function() {
  set.seed(123) # For reproducibility

  # Create complete data with valid values
  normal_data <- data.frame(
    patient_id = c("P001", "P002", "P003"),
    oab_1 = c(2, 4, 3),  # Symptom items (1-6)
    oab_2 = c(3, 5, 2),
    oab_3 = c(4, 3, 1),
    oab_4 = c(2, 4, 3),
    oab_5 = c(3, 2, 4),
    oab_6 = c(1, 3, 2),
    oab_7 = c(4, 2, 5),  # HRQL items (7-19)
    oab_8 = c(5, 3, 4),
    oab_9 = c(3, 4, 5),
    oab_10 = c(4, 3, 4),
    oab_11 = c(5, 2, 3),
    oab_12 = c(4, 3, 5),
    oab_13 = c(3, 4, 4),
    oab_14 = c(5, 3, 3),
    oab_15 = c(4, 5, 4),
    oab_16 = c(3, 4, 5),
    oab_17 = c(4, 3, 4),
    oab_18 = c(5, 4, 3),
    oab_19 = c(4, 5, 4)
  )

  # Create data with missing values
  missing_data <- normal_data
  missing_data$oab_1[3] <- NA
  missing_data$oab_3[2] <- NA
  missing_data$oab_9[2] <- NA
  missing_data$oab_11[3] <- NA
  missing_data$oab_14[2] <- NA
  missing_data$oab_18[3] <- NA

  # Create data with too many missing values
  heavy_missing_data <- normal_data
  # Set most symptom items to NA for first patient
  heavy_missing_data$oab_1[1] <- NA
  heavy_missing_data$oab_2[1] <- NA
  heavy_missing_data$oab_3[1] <- NA
  heavy_missing_data$oab_4[1] <- NA
  # Set most HRQL items to NA for third patient
  heavy_missing_data$oab_8[3] <- NA
  heavy_missing_data$oab_9[3] <- NA
  heavy_missing_data$oab_10[3] <- NA
  heavy_missing_data$oab_11[3] <- NA
  heavy_missing_data$oab_12[3] <- NA
  heavy_missing_data$oab_13[3] <- NA
  heavy_missing_data$oab_14[3] <- NA
  heavy_missing_data$oab_15[3] <- NA
  heavy_missing_data$oab_17[3] <- NA

  # Create data with invalid values
  invalid_data <- normal_data
  invalid_data$oab_2[1] <- 7  # Above valid range
  invalid_data$oab_5[3] <- 0  # Below valid range
  invalid_data$oab_9[2] <- -1 # Negative
  invalid_data$oab_13[1] <- 8 # Above valid range

  # Create data with alternate column naming
  alt_named_data <- normal_data
  colnames(alt_named_data) <- c("patient_id",
                                paste0("oabq_item_", 1:19))

  # Create data with character instead of numeric values
  char_data <- normal_data
  char_data$oab_1 <- as.character(char_data$oab_1)
  char_data$oab_10 <- as.character(char_data$oab_10)

  # Return all test datasets
  return(list(
    normal_data = normal_data,
    missing_data = missing_data,
    heavy_missing_data = heavy_missing_data,
    invalid_data = invalid_data,
    alt_named_data = alt_named_data,
    char_data = char_data
  ))
}

# Test basic functionality with complete data
test_that("calculate_oabq_sf works with complete data", {
  test_data <- create_test_data()

  # Test with complete data
  result <- calculate_oabq_sf(
    questionnaire_data = test_data$normal_data,
    id_column = "patient_id"
  )

  # Test output structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("symptom_score", "hrql_score", "symptom_valid", "hrql_valid", "total_valid") %in% colnames(result)))

  # Test data types
  expect_type(result$symptom_score, "double")
  expect_type(result$hrql_score, "double")
  expect_type(result$symptom_valid, "logical")
  expect_type(result$hrql_valid, "logical")
  expect_type(result$total_valid, "logical")

  # Test score calculation accuracy
  # Manually calculate scores for first patient
  p1_symptom_mean <- mean(unlist(test_data$normal_data[1, c("oab_1", "oab_2", "oab_3", "oab_4", "oab_5", "oab_6")]))
  p1_symptom_score <- ((p1_symptom_mean - 1) / 5) * 100
  p1_hrql_mean <- mean(unlist(test_data$normal_data[1, paste0("oab_", 7:19)]))
  p1_hrql_score <- ((p1_hrql_mean - 1) / 5) * 100

  expect_equal(result$symptom_score[1], p1_symptom_score, tolerance = 0.01)
  expect_equal(result$hrql_score[1], p1_hrql_score, tolerance = 0.01)

  # Test validity flags
  expect_true(all(result$symptom_valid))
  expect_true(all(result$hrql_valid))
  expect_true(all(result$total_valid))
})

# Test handling of missing data
test_that("calculate_oabq_sf handles missing data correctly", {
  test_data <- create_test_data()

  # Test with data containing some missing values
  result <- calculate_oabq_sf(
    questionnaire_data = test_data$missing_data,
    id_column = "patient_id"
  )

  # All rows should still be valid with default thresholds
  expect_true(all(result$symptom_valid))
  expect_true(all(result$hrql_valid))
  expect_true(all(result$total_valid))

  # Test with stricter missing thresholds
  strict_result <- calculate_oabq_sf(
    questionnaire_data = test_data$missing_data,
    id_column = "patient_id",
    symptom_missing_threshold = 0.1,  # Allow only 10% missing
    hrql_missing_threshold = 0.1
  )

  # Third row has missing symptom items and should be invalid
  expect_false(strict_result$symptom_valid[3])
  # Second row has missing HRQL items and should be invalid
  expect_false(strict_result$hrql_valid[2])
  # Both rows should be invalid in total
  expect_false(strict_result$total_valid[2])
  expect_false(strict_result$total_valid[3])

  # Test with data containing many missing values
  heavy_result <- calculate_oabq_sf(
    questionnaire_data = test_data$heavy_missing_data,
    id_column = "patient_id"
  )

  # First patient has too many missing symptom items
  expect_false(heavy_result$symptom_valid[1])
  # Third patient has too many missing HRQL items
  expect_false(heavy_result$hrql_valid[3])
  # Check for NA values in scores
  expect_true(is.na(heavy_result$symptom_score[1]))
  expect_true(is.na(heavy_result$hrql_score[3]))
})

# Test handling of invalid values
test_that("calculate_oabq_sf handles invalid data values", {
  test_data <- create_test_data()

  # Test with data containing invalid values
  result <- calculate_oabq_sf(
    questionnaire_data = test_data$invalid_data,
    id_column = "patient_id",
    verbose = TRUE
  )

  # Should clip values and produce valid results
  expect_true(all(result$symptom_valid))
  expect_true(all(result$hrql_valid))
  expect_true(all(result$total_valid))

  # Test with character data that should be converted
  char_result <- calculate_oabq_sf(
    questionnaire_data = test_data$char_data,
    id_column = "patient_id"
  )

  # Should convert to numeric and produce valid results
  expect_true(all(char_result$symptom_valid))
  expect_true(all(char_result$hrql_valid))
  expect_true(all(char_result$total_valid))
})

# Test with alternative column naming
test_that("calculate_oabq_sf works with alternative column naming", {
  test_data <- create_test_data()

  # Test with alternative column naming
  result <- calculate_oabq_sf(
    questionnaire_data = test_data$alt_named_data,
    id_column = "patient_id",
    item_prefix = "oabq_item_"
  )

  # Should work with alternative naming
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("symptom_score", "hrql_score") %in% colnames(result)))
  expect_true(all(result$symptom_valid))
  expect_true(all(result$hrql_valid))
})

# Test error handling and validation
test_that("calculate_oabq_sf handles errors correctly", {
  test_data <- create_test_data()

  # Test with non-data frame input
  expect_error(
    calculate_oabq_sf(questionnaire_data = "not_a_data_frame"),
    "questionnaire_data must be a data frame"
  )

  # Test with empty data frame
  expect_error(
    calculate_oabq_sf(questionnaire_data = data.frame()),
    "questionnaire_data must have at least one row"
  )

  # Test with invalid column name
  expect_error(
    calculate_oabq_sf(
      questionnaire_data = test_data$normal_data,
      id_column = "nonexistent_column"
    ),
    "id_column 'nonexistent_column' not found in questionnaire_data"
  )

  # Test with invalid missing threshold
  expect_error(
    calculate_oabq_sf(
      questionnaire_data = test_data$normal_data,
      symptom_missing_threshold = 1.5
    ),
    "symptom_missing_threshold must be a value between 0 and 1"
  )

  expect_error(
    calculate_oabq_sf(
      questionnaire_data = test_data$normal_data,
      hrql_missing_threshold = -0.1
    ),
    "hrql_missing_threshold must be a value between 0 and 1"
  )

  # Test with no matching columns
  data_no_match <- test_data$normal_data
  colnames(data_no_match) <- c("patient_id", paste0("wrong_prefix_", 1:19))

  expect_error(
    calculate_oabq_sf(
      questionnaire_data = data_no_match,
      id_column = "patient_id"
    ),
    "No OAB-q SF item columns found with prefix"
  )
})

# Test with custom symptom/HRQL item sets
test_that("calculate_oabq_sf works with custom item sets", {
  test_data <- create_test_data()

  # Test with custom symptom and HRQL item sets
  custom_result <- calculate_oabq_sf(
    questionnaire_data = test_data$normal_data,
    id_column = "patient_id",
    symptom_items = c(1, 3, 5),  # Only use odd-numbered symptom items
    hrql_items = c(8, 10, 12, 14, 16, 18)  # Only use even-numbered HRQL items
  )

  # Should use custom item sets
  expect_s3_class(custom_result, "data.frame")
  expect_equal(nrow(custom_result), 3)
  expect_true(all(c("symptom_score", "hrql_score") %in% colnames(custom_result)))

  # Manually calculate scores with custom item sets for first patient
  p1_symptom_mean <- mean(unlist(test_data$normal_data[1, c("oab_1", "oab_3", "oab_5")]))
  p1_symptom_score <- ((p1_symptom_mean - 1) / 5) * 100
  p1_hrql_mean <- mean(unlist(test_data$normal_data[1, c("oab_8", "oab_10", "oab_12", "oab_14", "oab_16", "oab_18")]))
  p1_hrql_score <- ((p1_hrql_mean - 1) / 5) * 100

  expect_equal(custom_result$symptom_score[1], p1_symptom_score, tolerance = 0.01)
  expect_equal(custom_result$hrql_score[1], p1_hrql_score, tolerance = 0.01)
})

# Test with edge case data
test_that("calculate_oabq_sf handles edge cases", {
  # Test with minimum valid values
  min_data <- data.frame(
    id = 1:3,
    oab_1 = c(1, 1, 1),
    oab_2 = c(1, 1, 1),
    oab_3 = c(1, 1, 1),
    oab_4 = c(1, 1, 1),
    oab_5 = c(1, 1, 1),
    oab_6 = c(1, 1, 1),
    oab_7 = c(1, 1, 1),
    oab_8 = c(1, 1, 1),
    oab_9 = c(1, 1, 1),
    oab_10 = c(1, 1, 1),
    oab_11 = c(1, 1, 1),
    oab_12 = c(1, 1, 1),
    oab_13 = c(1, 1, 1),
    oab_14 = c(1, 1, 1),
    oab_15 = c(1, 1, 1),
    oab_16 = c(1, 1, 1),
    oab_17 = c(1, 1, 1),
    oab_18 = c(1, 1, 1),
    oab_19 = c(1, 1, 1)
  )

  min_result <- calculate_oabq_sf(
    questionnaire_data = min_data,
    id_column = "id"
  )

  # All scores should be 0 (minimum possible value)
  expect_equal(min_result$symptom_score, c(0, 0, 0))
  expect_equal(min_result$hrql_score, c(0, 0, 0))

  # Test with maximum valid values
  max_data <- data.frame(
    id = 1:3,
    oab_1 = c(6, 6, 6),
    oab_2 = c(6, 6, 6),
    oab_3 = c(6, 6, 6),
    oab_4 = c(6, 6, 6),
    oab_5 = c(6, 6, 6),
    oab_6 = c(6, 6, 6),
    oab_7 = c(6, 6, 6),
    oab_8 = c(6, 6, 6),
    oab_9 = c(6, 6, 6),
    oab_10 = c(6, 6, 6),
    oab_11 = c(6, 6, 6),
    oab_12 = c(6, 6, 6),
    oab_13 = c(6, 6, 6),
    oab_14 = c(6, 6, 6),
    oab_15 = c(6, 6, 6),
    oab_16 = c(6, 6, 6),
    oab_17 = c(6, 6, 6),
    oab_18 = c(6, 6, 6),
    oab_19 = c(6, 6, 6)
  )

  max_result <- calculate_oabq_sf(
    questionnaire_data = max_data,
    id_column = "id"
  )

  # All scores should be 100 (maximum possible value)
  expect_equal(max_result$symptom_score, c(100, 100, 100))
  expect_equal(max_result$hrql_score, c(100, 100, 100))

  # Test with single row
  single_row_data <- max_data[1, , drop = FALSE]

  single_row_result <- calculate_oabq_sf(
    questionnaire_data = single_row_data,
    id_column = "id"
  )

  expect_equal(nrow(single_row_result), 1)
  expect_equal(single_row_result$symptom_score, 100)
  expect_equal(single_row_result$hrql_score, 100)
})

# Test verbose mode
test_that("calculate_oabq_sf verbose mode works", {
  test_data <- create_test_data()

  # Test with verbose mode on - should not error
  expect_error(
    calculate_oabq_sf(
      questionnaire_data = test_data$normal_data,
      id_column = "patient_id",
      verbose = TRUE
    ),
    NA
  )
})
