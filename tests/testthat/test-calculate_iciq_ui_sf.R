library(testthat)
library(dplyr)

# Create sample test data for use in multiple tests
create_test_data <- function() {
  set.seed(123) # For reproducibility

  # Create complete data with valid values
  normal_data <- data.frame(
    patient_id = c("P001", "P002", "P003"),
    iciq_1 = c(2, 3, 1),     # Frequency (0-5)
    iciq_2 = c(4, 2, 0),     # Volume (0-6)
    iciq_3 = c(6, 8, 3),     # Impact (0-10)
    iciq_4_1 = c(1, 0, 0),   # Situation: leaks before reaching toilet
    iciq_4_2 = c(0, 1, 0),   # Situation: leaks when coughing/sneezing
    iciq_4_3 = c(0, 0, 1),   # Situation: leaks when asleep
    iciq_4_4 = c(0, 1, 0),   # Situation: leaks with physical activity
    iciq_4_5 = c(1, 0, 0),   # Situation: leaks when finished urinating
    iciq_4_6 = c(0, 0, 0),   # Situation: leaks for no obvious reason
    iciq_4_7 = c(0, 0, 0)    # Situation: leaks all the time
  )

  # Create data with missing values
  missing_data <- normal_data
  missing_data$iciq_1[3] <- NA
  missing_data$iciq_3[2] <- NA
  missing_data$iciq_4_3[1] <- NA

  # Create data with invalid values
  invalid_data <- normal_data
  invalid_data$iciq_1[1] <- 7     # Above valid range
  invalid_data$iciq_2[3] <- -1    # Below valid range
  invalid_data$iciq_3[2] <- 12    # Above valid range

  # Create data with alternative column naming
  alt_named_data <- data.frame(
    record_id = c("R1", "R2", "R3"),
    frequency = c(2, 3, 1),      # Frequency (0-5)
    amount = c(4, 2, 0),         # Volume (0-6)
    qol_impact = c(6, 8, 3),     # Impact (0-10)
    sit_toilet = c(1, 0, 0),     # Situation: before reaching toilet
    sit_cough = c(0, 1, 0),      # Situation: when coughing/sneezing
    sit_sleep = c(0, 0, 1)       # Situation: when asleep
  )

  # Create data with character instead of numeric values
  char_data <- normal_data
  char_data$iciq_1 <- as.character(char_data$iciq_1)
  char_data$iciq_3 <- as.character(char_data$iciq_3)

  # Create minimal valid data (without situation columns)
  minimal_data <- data.frame(
    id = 1:3,
    iciq_1 = c(1, 4, 0),
    iciq_2 = c(3, 1, 0),
    iciq_3 = c(5, 7, 0)
  )

  # Return all test datasets
  return(list(
    normal_data = normal_data,
    missing_data = missing_data,
    invalid_data = invalid_data,
    alt_named_data = alt_named_data,
    char_data = char_data,
    minimal_data = minimal_data
  ))
}

# Test basic functionality with complete data
test_that("calculate_iciq_ui_sf works with complete data", {
  test_data <- create_test_data()

  # Test with complete data
  result <- calculate_iciq_ui_sf(
    questionnaire_data = test_data$normal_data,
    id_column = "patient_id"
  )

  # Test output structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("iciq_score", "frequency_score", "volume_score", "impact_score",
                    "iciq_missing", "iciq_valid") %in% colnames(result)))

  # Test data types
  expect_type(result$iciq_score, "double")
  expect_type(result$frequency_score, "double")
  expect_type(result$volume_score, "double")
  expect_type(result$impact_score, "double")
  expect_type(result$iciq_missing, "integer")
  expect_type(result$iciq_valid, "logical")

  # Test score calculation accuracy
  # Patient 1: 2 + 4 + 6 = 12
  expect_equal(result$iciq_score[1], 12)
  # Patient 2: 3 + 2 + 8 = 13
  expect_equal(result$iciq_score[2], 13)
  # Patient 3: 1 + 0 + 3 = 4
  expect_equal(result$iciq_score[3], 4)

  # Test situation columns exist
  situation_cols <- grep("^situation_", names(result), value = TRUE)
  expect_equal(length(situation_cols), 7) # Should have 7 situation columns

  # Test all rows are valid
  expect_true(all(result$iciq_valid))
  expect_equal(sum(result$iciq_missing), 0) # No missing values
})

# Test handling of missing data
test_that("calculate_iciq_ui_sf handles missing data correctly", {
  test_data <- create_test_data()

  # Test with default missing threshold (0)
  result <- calculate_iciq_ui_sf(
    questionnaire_data = test_data$missing_data,
    id_column = "patient_id"
  )

  # Rows with missing values should be invalid with threshold = 0
  expect_false(result$iciq_valid[2]) # Missing iciq_3
  expect_false(result$iciq_valid[3]) # Missing iciq_1

  # Check missing value count
  expect_equal(result$iciq_missing[1], 0)
  expect_equal(result$iciq_missing[2], 1)
  expect_equal(result$iciq_missing[3], 1)

  # Check for NA in scores for invalid rows
  expect_true(is.na(result$iciq_score[2]))
  expect_true(is.na(result$iciq_score[3]))

  # Test with higher missing threshold (0.34 = 1/3)
  result_threshold <- calculate_iciq_ui_sf(
    questionnaire_data = test_data$missing_data,
    id_column = "patient_id",
    missing_threshold = 0.34
  )

  # All rows should be valid with threshold = 0.34
  expect_true(all(result_threshold$iciq_valid))

  # Check for imputed scores
  expect_false(is.na(result_threshold$iciq_score[2]))
  expect_false(is.na(result_threshold$iciq_score[3]))
})

# Test handling of invalid values
test_that("calculate_iciq_ui_sf handles invalid data values", {
  test_data <- create_test_data()

  # Test with data containing invalid values
  result <- calculate_iciq_ui_sf(
    questionnaire_data = test_data$invalid_data,
    id_column = "patient_id",
    verbose = TRUE
  )

  # Test that invalid values are clipped to valid ranges
  expect_equal(result$frequency_score[1], 5) # Clipped from 7 to 5
  expect_equal(result$volume_score[3], 0)    # Clipped from -1 to 0
  expect_equal(result$impact_score[2], 10)   # Clipped from 12 to 10

  # Recalculate expected scores with clipped values
  expect_equal(result$iciq_score[1], 5 + 4 + 6) # 15
  expect_equal(result$iciq_score[2], 3 + 2 + 10) # 15
  expect_equal(result$iciq_score[3], 1 + 0 + 3) # 4
})

# Test with alternative column naming
test_that("calculate_iciq_ui_sf works with alternative column naming", {
  test_data <- create_test_data()

  # Test with alternative column naming
  result <- calculate_iciq_ui_sf(
    questionnaire_data = test_data$alt_named_data,
    id_column = "record_id",
    frequency_item = "frequency",
    volume_item = "amount",
    impact_item = "qol_impact",
    situation_prefix = "sit_"
  )

  # Test output structure and correct ID column
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true("record_id" %in% colnames(result))

  # Test score calculation with alternative column names
  # R1: 2 + 4 + 6 = 12
  expect_equal(result$iciq_score[1], 12)
  # R2: 3 + 2 + 8 = 13
  expect_equal(result$iciq_score[2], 13)
  # R3: 1 + 0 + 3 = 4
  expect_equal(result$iciq_score[3], 4)

  # Test situation columns with alternative prefix
  situation_cols <- grep("^situation_", names(result), value = TRUE)
  expect_equal(length(situation_cols), 3) # Should have 3 situation columns
})

# Test handling of character data conversion
test_that("calculate_iciq_ui_sf handles character data conversion", {
  test_data <- create_test_data()

  # Test with character data that should be converted
  result <- calculate_iciq_ui_sf(
    questionnaire_data = test_data$char_data,
    id_column = "patient_id"
  )

  # Test that character data was correctly converted to numeric
  expect_equal(result$frequency_score[1], 2)
  expect_equal(result$impact_score[3], 3)

  # Test score calculation after character conversion
  # Patient 1: 2 + 4 + 6 = 12
  expect_equal(result$iciq_score[1], 12)
})

# Test without situation analysis
test_that("calculate_iciq_ui_sf works without situation analysis", {
  test_data <- create_test_data()

  # Test with calculate_situations = FALSE
  result <- calculate_iciq_ui_sf(
    questionnaire_data = test_data$normal_data,
    id_column = "patient_id",
    calculate_situations = FALSE
  )

  # Test output structure
  expect_s3_class(result, "data.frame")

  # Ensure no situation columns are present
  situation_cols <- grep("^situation_", names(result), value = TRUE)
  expect_equal(length(situation_cols), 0)

  # Core functionality should still work
  expect_equal(result$iciq_score[1], 12)
})

# Test error handling
test_that("calculate_iciq_ui_sf handles errors correctly", {
  test_data <- create_test_data()

  # Test with non-data frame input
  expect_error(
    calculate_iciq_ui_sf(questionnaire_data = "not_a_data_frame"),
    "questionnaire_data must be a data frame"
  )

  # Test with empty data frame
  expect_error(
    calculate_iciq_ui_sf(questionnaire_data = data.frame()),
    "questionnaire_data must have at least one row"
  )

  # Test with nonexistent ID column
  expect_error(
    calculate_iciq_ui_sf(
      questionnaire_data = test_data$normal_data,
      id_column = "nonexistent_column"
    ),
    "id_column 'nonexistent_column' not found in questionnaire_data"
  )

  # Test with missing required columns
  missing_col_data <- test_data$normal_data[, !names(test_data$normal_data) %in% "iciq_2"]
  expect_error(
    calculate_iciq_ui_sf(questionnaire_data = missing_col_data),
    "Missing required ICIQ-UI SF columns: iciq_2"
  )

  # Test with invalid missing threshold
  expect_error(
    calculate_iciq_ui_sf(
      questionnaire_data = test_data$normal_data,
      missing_threshold = 1.5
    ),
    "missing_threshold must be a value between 0 and 1"
  )
})

# Test minimal data (without situation columns)
test_that("calculate_iciq_ui_sf works with minimal data", {
  test_data <- create_test_data()

  # Test with minimal data (no situation columns)
  result <- calculate_iciq_ui_sf(
    questionnaire_data = test_data$minimal_data,
    id_column = "id"
  )

  # Test output structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)

  # Test score calculation
  # Row 1: 1 + 3 + 5 = 9
  expect_equal(result$iciq_score[1], 9)
  # Row 2: 4 + 1 + 7 = 12
  expect_equal(result$iciq_score[2], 12)
  # Row 3: 0 + 0 + 0 = 0
  expect_equal(result$iciq_score[3], 0)

  # Ensure no errors with no situation columns
  situation_cols <- grep("^situation_", names(result), value = TRUE)
  expect_equal(length(situation_cols), 0) # Should have no situation columns
})

# Test verbose mode
test_that("calculate_iciq_ui_sf verbose mode works", {
  test_data <- create_test_data()

  # Test with verbose mode - should not error
  expect_error(
    calculate_iciq_ui_sf(
      questionnaire_data = test_data$normal_data,
      id_column = "patient_id",
      verbose = TRUE
    ),
    NA
  )
})

# Test severity classification
test_that("calculate_iciq_ui_sf scores classify severity correctly", {
  # Create data with scores in each severity range
  severity_data <- data.frame(
    id = 1:3,
    iciq_1 = c(1, 3, 5),
    iciq_2 = c(1, 2, 6),
    iciq_3 = c(1, 5, 10)
  )

  result <- calculate_iciq_ui_sf(
    questionnaire_data = severity_data,
    id_column = "id"
  )

  # Patient 1: 1+1+1 = 3 (Mild: 1-5)
  expect_equal(result$iciq_score[1], 3)
  # Patient 2: 3+2+5 = 10 (Moderate: 6-12)
  expect_equal(result$iciq_score[2], 10)
  # Patient 3: 5+6+10 = 21 (Severe: 13-21)
  expect_equal(result$iciq_score[3], 21)

  # Manually check severity classification based on score ranges
  # (This doesn't test a function feature but verifies the scoring interpretation)
  severity <- ifelse(
    result$iciq_score <= 5, "Mild",
    ifelse(result$iciq_score <= 12, "Moderate", "Severe")
  )

  expect_equal(severity, c("Mild", "Moderate", "Severe"))
})

