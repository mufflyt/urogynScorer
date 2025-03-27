test_that("calculate_pfdi_scores works with standard column names", {
  # Create sample test data
  test_data <- tibble::tibble(
    Record_ID = 1:5,
    `PFDI-1` = c(1, 2, 3, NA, 1),
    `PFDI-2` = c(2, 2, 3, 1, 2),
    `PFDI-3` = c(2, 1, 2, 2, 3),
    `PFDI-4` = c(3, 0, 1, 3, 2),
    `PFDI-5` = c(1, 1, 1, 2, 1),
    `PFDI-6` = c(0, 2, 2, 1, 0),
    `PFDI-7` = c(1, 3, 2, 0, 1),
    `PFDI-8` = c(2, 2, 1, 1, 2),
    `PFDI-9` = c(3, 1, 0, 2, 3),
    `PFDI-10` = c(2, 0, 1, 3, 2),
    `PFDI-11` = c(1, 1, 2, 2, 1),
    `PFDI-12` = c(0, 2, 3, 1, 0),
    `PFDI-13` = c(1, 3, 2, 0, 1),
    `PFDI-14` = c(2, 2, 1, 1, 2),
    `PFDI-15` = c(3, 1, 0, 2, 3),
    `PFDI-16` = c(2, 0, 1, 3, 2),
    `PFDI-17` = c(1, 1, 2, 2, 1),
    `PFDI-18` = c(0, 2, 3, 1, 0),
    `PFDI-19` = c(1, 3, 2, 0, 1),
    `PFDI-20` = c(2, 2, 1, 1, 2)
  )

  # Calculate scores
  scores <- calculate_pfdi_scores(test_data, verbose = TRUE)

  # Test that output has the right structure
  expect_true(is.data.frame(scores))
  expect_equal(nrow(scores), 5)
  expect_equal(colnames(scores), c("Record_ID", "POPDI_6", "CRADI_8", "UDI_6", "PFDI_20_total"))

  # Test that scores are calculated correctly for the first patient
  patient1 <- scores %>% dplyr::filter(Record_ID == 1)
  expect_equal(patient1$POPDI_6, 37.5, tolerance = 0.01) # 1.5*25 = 37.5
  expect_equal(patient1$CRADI_8, 37.5, tolerance = 0.01) # 1.5*25 = 37.5
  expect_equal(patient1$UDI_6, 37.5, tolerance = 0.01) # 1.5*25 = 37.5
  expect_equal(patient1$PFDI_20_total, 112.5, tolerance = 0.01) # 37.5+37.5+37.5 = 112.5

  # Test handling of missing values
  patient4 <- scores %>% dplyr::filter(Record_ID == 4)
  expect_false(is.na(patient4$POPDI_6)) # Only 1/6 missing, should still calculate

  # Test with custom missing threshold
  strict_scores <- calculate_pfdi_scores(test_data, missing_threshold = 0)
  strict_patient4 <- strict_scores %>% dplyr::filter(Record_ID == 4)
  expect_true(is.na(strict_patient4$POPDI_6)) # Any missing should result in NA
})

test_that("calculate_pfdi_scores works with alternative column names", {
  # Create sample test data with alternative column names
  test_data <- tibble::tibble(
    Record_ID = 1:3,
    `PFDI-POPDI-1` = c(1, 2, 3),
    `PFDI-POPDI-2` = c(2, 2, 3),
    `PFDI-POPDI-3` = c(2, 1, 2),
    `PFDI-POPDI-4` = c(3, 0, 1),
    `PFDI-POPDI-5` = c(1, 1, 1),
    `PFDI-POPDI-6` = c(0, 2, 2),
    `PFDI-CRADI-1` = c(1, 3, 2),
    `PFDI-CRADI-2` = c(2, 2, 1),
    `PFDI-CRADI-3` = c(3, 1, 0),
    `PFDI-CRADI-4` = c(2, 0, 1),
    `PFDI-CRADI-5` = c(1, 1, 2),
    `PFDI-CRADI-6` = c(0, 2, 3),
    `PFDI-CRADI-7` = c(1, 3, 2),
    `PFDI-CRADI-8` = c(2, 2, 1),
    `PFDI-UDI-1` = c(3, 1, 0),
    `PFDI-UDI-2` = c(2, 0, 1),
    `PFDI-UDI-3` = c(1, 1, 2),
    `PFDI-UDI-4` = c(0, 2, 3),
    `PFDI-UDI-5` = c(1, 3, 2),
    `PFDI-UDI-6` = c(2, 2, 1)
  )

  # Calculate scores
  scores <- calculate_pfdi_scores(test_data)

  # Test that output has the right structure
  expect_true(is.data.frame(scores))
  expect_equal(nrow(scores), 3)
  expect_equal(colnames(scores), c("Record_ID", "POPDI_6", "CRADI_8", "UDI_6", "PFDI_20_total"))
})

test_that("calculate_pfdi_scores correctly handles output_file parameter", {
  # Create sample test data
  test_data <- tibble::tibble(
    Record_ID = 1:3,
    `PFDI-1` = c(1, 2, 3),
    `PFDI-2` = c(2, 2, 3),
    `PFDI-3` = c(2, 1, 2),
    `PFDI-4` = c(3, 0, 1),
    `PFDI-5` = c(1, 1, 1),
    `PFDI-6` = c(0, 2, 2),
    `PFDI-7` = c(1, 3, 2),
    `PFDI-8` = c(2, 2, 1),
    `PFDI-9` = c(3, 1, 0),
    `PFDI-10` = c(2, 0, 1),
    `PFDI-11` = c(1, 1, 2),
    `PFDI-12` = c(0, 2, 3),
    `PFDI-13` = c(1, 3, 2),
    `PFDI-14` = c(2, 2, 1),
    `PFDI-15` = c(3, 1, 0),
    `PFDI-16` = c(2, 0, 1),
    `PFDI-17` = c(1, 1, 2),
    `PFDI-18` = c(0, 2, 3),
    `PFDI-19` = c(1, 3, 2),
    `PFDI-20` = c(2, 2, 1)
  )

  # Create a temporary file for testing
  temp_file <- tempfile(fileext = ".csv")

  # Calculate scores and save to the temp file
  scores <- calculate_pfdi_scores(test_data, output_file = temp_file)

  # Check that the file exists and has the correct content
  expect_true(file.exists(temp_file))
  file_content <- utils::read.csv(temp_file)
  expect_equal(nrow(file_content), 3)
  expect_equal(colnames(file_content), c("Record_ID", "POPDI_6", "CRADI_8", "UDI_6", "PFDI_20_total"))

  # Clean up
  file.remove(temp_file)
})

