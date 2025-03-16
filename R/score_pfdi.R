#' Score the Pelvic Floor Distress Inventory (PFDI-20)
#'
#' @description
#' This function calculates scores for the Pelvic Floor Distress Inventory
#' (PFDI-20), including subscale scores (POPDI-6, CRADI-8, UDI-6) and the
#' total score.
#'
#' @param questionnaire_data A data frame containing responses to PFDI-20 items.
#' @param popdi_cols Character vector of column names containing POPDI-6 items.
#' @param cradi_cols Character vector of column names containing CRADI-8 items.
#' @param udi_cols Character vector of column names containing UDI-6 items.
#' @param verbose Logical. If TRUE, display detailed logging. Default is FALSE.
#'
#' @return A data frame containing the original data plus additional columns for
#'   POPDI-6, CRADI-8, UDI-6 scores, and the total PFDI-20 score.
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate select bind_cols
#' @importFrom tidyr replace_na
#' @importFrom purrr map_dbl
#' @importFrom logger log_info log_debug log_error
#'
#' @examples
#' # Example 1: Basic usage with sample data
#' data("pfdi_data")
#' popdi_cols <- grep("^PFDI-.*POPDI", colnames(pfdi_data), value = TRUE)
#' cradi_cols <- grep("^PFDI-.*CRADI", colnames(pfdi_data), value = TRUE)
#' udi_cols <- grep("^PFDI-.*UDI", colnames(pfdi_data), value = TRUE)
#' scored_results <- score_pfdi(pfdi_data, popdi_cols, cradi_cols, udi_cols)
#' head(scored_results)
#'
#' # Example 2: With custom column names
#' popdi_custom <- c("q1_popdi", "q2_popdi", "q3_popdi", "q4_popdi", "q5_popdi",
#'                   "q6_popdi")
#' cradi_custom <- c("q7_cradi", "q8_cradi", "q9_cradi", "q10_cradi", "q11_cradi",
#'                   "q12_cradi", "q13_cradi", "q14_cradi")
#' udi_custom <- c("q15_udi", "q16_udi", "q17_udi", "q18_udi", "q19_udi", "q20_udi")
#' sample_data <- data.frame(
#'   id = 1:3,
#'   q1_popdi = c(3, 2, 1), q2_popdi = c(2, 1, 2), q3_popdi = c(1, 0, 3),
#'   q4_popdi = c(2, 2, 1), q5_popdi = c(0, 1, 2), q6_popdi = c(3, 2, 1),
#'   q7_cradi = c(1, 0, 2), q8_cradi = c(2, 1, 1), q9_cradi = c(0, 2, 0),
#'   q10_cradi = c(3, 1, 2), q11_cradi = c(1, 0, 1), q12_cradi = c(2, 2, 0),
#'   q13_cradi = c(1, 1, 3), q14_cradi = c(0, 0, 1),
#'   q15_udi = c(2, 1, 0), q16_udi = c(1, 2, 1), q17_udi = c(3, 0, 2),
#'   q18_udi = c(0, 1, 3), q19_udi = c(2, 2, 0), q20_udi = c(1, 3, 1)
#' )
#' scored_custom <- score_pfdi(sample_data, popdi_custom, cradi_custom,
#'                             udi_custom)
#' head(scored_custom)
#'
#' # Example 3: With verbose logging
#' scored_verbose <- score_pfdi(pfdi_data, popdi_cols, cradi_cols, udi_cols,
#'                              verbose = TRUE)
#' head(scored_verbose)
#'
#' @export
score_pfdi <- function(questionnaire_data, popdi_cols, cradi_cols, udi_cols,
                       verbose = FALSE) {
  # Setup logging based on verbose parameter
  if (verbose) {
    logger::log_threshold(logger::INFO)
  } else {
    logger::log_threshold(logger::ERROR)
  }

  # Log function call
  logger::log_info("Starting PFDI-20 scoring")
  logger::log_debug("Input parameters: {length(popdi_cols)} POPDI columns,
                    {length(cradi_cols)} CRADI columns,
                    {length(udi_cols)} UDI columns")

  # Validate inputs
  validate_pfdi_inputs(questionnaire_data, popdi_cols, cradi_cols, udi_cols)

  # Calculate subscale scores
  logger::log_info("Calculating POPDI-6 score")
  popdi_results <- calculate_subscale_score(questionnaire_data, popdi_cols,
                                            "POPDI_6", multiplier = 25/6)

  logger::log_info("Calculating CRADI-8 score")
  cradi_results <- calculate_subscale_score(questionnaire_data, cradi_cols,
                                            "CRADI_8", multiplier = 25/8)

  logger::log_info("Calculating UDI-6 score")
  udi_results <- calculate_subscale_score(questionnaire_data, udi_cols,
                                          "UDI_6", multiplier = 25/6)

  # Calculate total PFDI-20 score
  logger::log_info("Calculating total PFDI-20 score")
  total_score <- popdi_results[[2]] + cradi_results[[2]] + udi_results[[2]]

  # Combine all scores with original data
  scored_data <- dplyr::bind_cols(
    questionnaire_data,
    popdi_results[[1]],
    cradi_results[[1]],
    udi_results[[1]],
    data.frame(PFDI_20_Total = total_score)
  )

  logger::log_info("PFDI-20 scoring completed successfully")
  logger::log_debug("Output data dimensions: {nrow(scored_data)} rows x {ncol(scored_data)} columns")

  return(scored_data)
}

#' Validate inputs for PFDI-20 scoring
#'
#' @param questionnaire_data Data frame with PFDI-20 responses
#' @param popdi_cols POPDI-6 column names
#' @param cradi_cols CRADI-8 column names
#' @param udi_cols UDI-6 column names
#'
#' @return No return value, called for side effects (validation)
#'
#' @noRd
validate_pfdi_inputs <- function(questionnaire_data, popdi_cols, cradi_cols, udi_cols) {
  # Check if questionnaire_data is a data frame
  assertthat::assert_that(is.data.frame(questionnaire_data),
                          msg = "questionnaire_data must be a data frame")

  # Check if column name vectors are character vectors
  assertthat::assert_that(is.character(popdi_cols),
                          msg = "popdi_cols must be a character vector")
  assertthat::assert_that(is.character(cradi_cols),
                          msg = "cradi_cols must be a character vector")
  assertthat::assert_that(is.character(udi_cols),
                          msg = "udi_cols must be a character vector")

  # Check if the expected number of columns are provided
  assertthat::assert_that(length(popdi_cols) == 6,
                          msg = "POPDI-6 requires exactly 6 items")
  assertthat::assert_that(length(cradi_cols) == 8,
                          msg = "CRADI-8 requires exactly 8 items")
  assertthat::assert_that(length(udi_cols) == 6,
                          msg = "UDI-6 requires exactly 6 items")

  # Check if all columns exist in the data frame
  all_cols <- c(popdi_cols, cradi_cols, udi_cols)
  missing_cols <- all_cols[!all_cols %in% colnames(questionnaire_data)]

  assertthat::assert_that(length(missing_cols) == 0,
                          msg = paste("The following columns are missing from the data:",
                                      paste(missing_cols, collapse = ", ")))

  # Check for valid values in the data (should be 0-4)
  for (col in all_cols) {
    values <- questionnaire_data[[col]]
    invalid_values <- values[!is.na(values) & !(values %in% 0:4)]

    if (length(invalid_values) > 0) {
      logger::log_error("Column {col} contains invalid values. PFDI-20 items should be scored 0-4.")
      assertthat::assert_that(length(invalid_values) == 0,
                              msg = paste("Column", col, "contains invalid values. PFDI-20 items should be scored 0-4."))
    }
  }
}

#' Calculate subscale score for PFDI-20
#'
#' @param questionnaire_data Data frame with PFDI-20 responses
#' @param subscale_cols Column names for the subscale
#' @param score_name Name to assign to the output score
#' @param multiplier Scaling multiplier for the subscale
#'
#' @return List containing: 1) a data frame with mean score and scaled score,
#'         2) the vector of scaled scores
#'
#' @noRd
calculate_subscale_score <- function(questionnaire_data, subscale_cols,
                                     score_name, multiplier) {
  # Calculate mean score for each row (patient)
  mean_scores <- purrr::map_dbl(1:nrow(questionnaire_data), function(i) {
    values <- as.numeric(questionnaire_data[i, subscale_cols])
    # Replace any NA values with the mean of non-NA values
    if (all(is.na(values))) {
      return(NA_real_)
    } else {
      mean(values, na.rm = TRUE)
    }
  })

  # Calculate scaled scores
  scaled_scores <- mean_scores * multiplier

  # Create data frame with results
  result_df <- data.frame(
    temp_mean = mean_scores,
    temp_scaled = scaled_scores
  )
  names(result_df) <- c(
    paste0(score_name, "_Mean"),
    paste0(score_name, "_Score")
  )

  logger::log_debug("Calculated {score_name} scores: range {min(scaled_scores, na.rm = TRUE)} - {max(scaled_scores, na.rm = TRUE)}")

  return(list(result_df, scaled_scores))
}

#' Example data for the Pelvic Floor Distress Inventory (PFDI-20)
#'
#' A dataset containing simulated responses to the PFDI-20 questionnaire
#' for demonstration purposes.
#'
#' @format A data frame with 20 rows and 21 columns:
#' \describe{
#'   \item{patient_id}{Unique identifier for each patient}
#'   \item{PFDI-01-POPDI}{Usually experience pressure in the lower abdomen?}
#'   \item{PFDI-02-POPDI}{Usually experience heaviness or dullness in the pelvic area?}
#'   \item{PFDI-03-POPDI}{Usually have a bulge or something falling out?}
#'   \item{PFDI-04-POPDI}{Usually have to push on the vagina or around the rectum?}
#'   \item{PFDI-05-POPDI}{Usually experience a feeling of incomplete bladder emptying?}
#'   \item{PFDI-06-POPDI}{Ever have to push up on a bulge in the vaginal area?}
#'   \item{PFDI-07-CRADI}{Feel you need to strain hard to have a bowel movement?}
#'   \item{PFDI-08-CRADI}{Feel you have not completely emptied your bowels?}
#'   \item{PFDI-09-CRADI}{Usually lose stool beyond your control?}
#'   \item{PFDI-10-CRADI}{Usually lose gas from the rectum beyond your control?}
#'   \item{PFDI-11-CRADI}{Usually have pain when you pass your stool?}
#'   \item{PFDI-12-CRADI}{Experience a strong sense of urgency to have a bowel movement?}
#'   \item{PFDI-13-CRADI}{Does part of your bowel ever pass through the rectum?}
#'   \item{PFDI-14-CRADI}{Usually experience frequent bowel movements?}
#'   \item{PFDI-15-UDI}{Usually experience urine leakage related to feeling of urgency?}
#'   \item{PFDI-16-UDI}{Usually experience urine leakage related to coughing, sneezing, or laughing?}
#'   \item{PFDI-17-UDI}{Usually experience small amounts of urine leakage?}
#'   \item{PFDI-18-UDI}{Usually experience difficulty emptying your bladder?}
#'   \item{PFDI-19-UDI}{Usually experience pain or discomfort in the lower abdomen or genital region?}
#'   \item{PFDI-20-UDI}{Usually experience heaviness or pressure in the pelvic area?}
#' }
"pfdi_data"
