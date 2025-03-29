#' Calculate PFIQ-7 (Pelvic Floor Impact Questionnaire) Scores
#'
#' @description
#' Calculates domain-specific and total scores from the PFIQ-7 questionnaire,
#' which measures the impact of pelvic floor disorders on quality of life across
#' three domains: urinary (UIQ-7), colorectal-anal (CRAIQ-7), and pelvic organ
#' prolapse (POPIQ-7).
#'
#' @param questionnaire_data A data frame containing the PFIQ-7 responses with
#'   21 columns (7 questions × 3 domains). Column names should follow the pattern
#'   'uiq1', 'uiq2', ..., 'craiq1', 'craiq2', ..., 'popiq1', 'popiq2', etc.
#' @param verbose Logical indicating whether to print detailed logging
#'   information during calculation. Default is FALSE.
#'
#' @return A data frame with calculated scores for each domain (UIQ-7, CRAIQ-7,
#'   POPIQ-7) and the total PFIQ-7 score. Each score ranges from 0 to 100, with
#'   higher scores indicating greater impact on quality of life.
#'
#' @details
#' The PFIQ-7 consists of three scales with 7 questions each:
#' - UIQ-7: Questions about urinary symptoms
#' - CRAIQ-7: Questions about colorectal-anal symptoms
#' - POPIQ-7: Questions about pelvic organ prolapse symptoms
#'
#' Each question is scored from 0 to 3:
#' - 0: Not at all
#' - 1: Somewhat
#' - 2: Moderately
#' - 3: Quite a bit
#'
#' Domain scores are calculated by: (sum of domain item scores) * (100/21)
#' Total PFIQ-7 score is the sum of all three domain scores (range 0-300)
#'
#' @examples
#' # Example 1: Basic usage with minimal data
#' sample_data <- data.frame(
#'   uiq1 = c(1, 2, 0), uiq2 = c(2, 1, 1), uiq3 = c(0, 0, 0),
#'   uiq4 = c(1, 2, 1), uiq5 = c(3, 2, 0), uiq6 = c(0, 1, 0),
#'   uiq7 = c(2, 2, 1),
#'   craiq1 = c(0, 1, 0), craiq2 = c(1, 0, 0), craiq3 = c(0, 0, 0),
#'   craiq4 = c(0, 1, 0), craiq5 = c(0, 0, 0), craiq6 = c(1, 0, 0),
#'   craiq7 = c(0, 0, 0),
#'   popiq1 = c(2, 3, 0), popiq2 = c(1, 2, 0), popiq3 = c(2, 1, 0),
#'   popiq4 = c(0, 0, 0), popiq5 = c(1, 0, 0), popiq6 = c(2, 1, 0),
#'   popiq7 = c(3, 2, 0)
#' )
#' calculate_pfiq7(sample_data)
#'
#' # Example 2: Using verbose mode for detailed logging
#' sample_data_2 <- data.frame(
#'   uiq1 = c(2, 3), uiq2 = c(1, 3), uiq3 = c(1, 2),
#'   uiq4 = c(2, 3), uiq5 = c(1, 2), uiq6 = c(0, 2),
#'   uiq7 = c(0, 1),
#'   craiq1 = c(0, 1), craiq2 = c(0, 0), craiq3 = c(1, 0),
#'   craiq4 = c(0, 0), craiq5 = c(0, 0), craiq6 = c(0, 1),
#'   craiq7 = c(1, 0),
#'   popiq1 = c(0, 1), popiq2 = c(0, 0), popiq3 = c(0, 0),
#'   popiq4 = c(0, 1), popiq5 = c(0, 0), popiq6 = c(0, 0),
#'   popiq7 = c(0, 0)
#' )
#' calculate_pfiq7(sample_data_2, verbose = TRUE)
#'
#' # Example 3: Handling missing data with verbose logging
#' sample_data_3 <- data.frame(
#'   uiq1 = c(1, 2, NA), uiq2 = c(2, 1, 1), uiq3 = c(0, 0, 0),
#'   uiq4 = c(1, NA, 1), uiq5 = c(3, 2, 0), uiq6 = c(0, 1, 0),
#'   uiq7 = c(2, 2, 1),
#'   craiq1 = c(0, 1, 0), craiq2 = c(NA, 0, 0), craiq3 = c(0, 0, 0),
#'   craiq4 = c(0, 1, NA), craiq5 = c(0, 0, 0), craiq6 = c(1, 0, 0),
#'   craiq7 = c(0, 0, 0),
#'   popiq1 = c(2, 3, 0), popiq2 = c(1, NA, 0), popiq3 = c(2, 1, 0),
#'   popiq4 = c(0, 0, 0), popiq5 = c(1, 0, NA), popiq6 = c(2, 1, 0),
#'   popiq7 = c(NA, 2, 0)
#' )
#' calculate_pfiq7(sample_data_3, verbose = TRUE)
#'
#' @importFrom logger log_info log_debug log_warn log_error
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate select summarise group_by
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map
#' @importFrom tibble tibble
#'
#' @export
calculate_pfiq7 <- function(questionnaire_data, verbose = FALSE) {
  # Initialize logger configuration
  setup_pfiq7_logger(verbose)

  # Log function call
  logger::log_info("Starting PFIQ-7 calculation")
  logger::log_debug("Input questionnaire_data dimensions: {nrow(questionnaire_data)} rows, {ncol(questionnaire_data)} columns")

  # Validate input data
  validated_data <- validate_pfiq7_data(questionnaire_data)

  # Calculate domain scores
  domain_scores <- calculate_domain_scores(validated_data)

  # Calculate total score
  pfiq7_scores <- calculate_total_score(domain_scores)

  logger::log_info("PFIQ-7 calculation completed successfully")
  logger::log_debug("Output pfiq7_scores dimensions: {nrow(pfiq7_scores)} rows, {ncol(pfiq7_scores)} columns")

  return(pfiq7_scores)
}

#' @noRd
setup_pfiq7_logger <- function(verbose) {
  if (verbose) {
    logger::log_threshold(logger::DEBUG)
    logger::log_debug("Verbose logging enabled")
  } else {
    logger::log_threshold(logger::INFO)
  }
}

#' @noRd
validate_pfiq7_data <- function(questionnaire_data) {
  logger::log_debug("Validating input data")

  # Check if input is a data frame
  assertthat::assert_that(is.data.frame(questionnaire_data),
                          msg = "Input must be a data frame")

  # Check for expected columns
  expected_prefixes <- c("uiq", "craiq", "popiq")
  expected_numbers <- 1:7
  expected_columns <- c()

  for (prefix in expected_prefixes) {
    for (num in expected_numbers) {
      expected_columns <- c(expected_columns, paste0(prefix, num))
    }
  }

  missing_columns <- expected_columns[!expected_columns %in% names(questionnaire_data)]

  if (length(missing_columns) > 0) {
    logger::log_warn("Missing expected columns: {paste(missing_columns, collapse = ', ')}")
    logger::log_warn("Will only use available columns for calculations")
  }

  # Check data range and convert to numeric if needed
  pfiq7_data <- questionnaire_data

  for (col in names(pfiq7_data)) {
    if (col %in% expected_columns) {
      # Convert to numeric if not already
      if (!is.numeric(pfiq7_data[[col]])) {
        logger::log_warn("Converting column {col} to numeric")
        pfiq7_data[[col]] <- as.numeric(pfiq7_data[[col]])
      }

      # Check for values outside valid range (0-3)
      invalid_values <- pfiq7_data[[col]][!is.na(pfiq7_data[[col]])]
      invalid_values <- invalid_values[invalid_values < 0 | invalid_values > 3]

      if (length(invalid_values) > 0) {
        logger::log_warn("Column {col} contains values outside valid range (0-3): {paste(invalid_values, collapse = ', ')}")

        # Clip values to valid range
        pfiq7_data[[col]][!is.na(pfiq7_data[[col]]) & pfiq7_data[[col]] < 0] <- 0
        pfiq7_data[[col]][!is.na(pfiq7_data[[col]]) & pfiq7_data[[col]] > 3] <- 3
        logger::log_warn("Invalid values in column {col} have been clipped to range 0-3")
      }
    }
  }

  logger::log_debug("Data validation completed")
  return(pfiq7_data)
}

#' @noRd
calculate_domain_scores <- function(pfiq7_data) {
  logger::log_debug("Calculating domain scores")

  domains <- c("uiq", "craiq", "popiq")
  domain_results <- tibble::tibble(
    patient_id = 1:nrow(pfiq7_data),
    uiq7_score = 0,
    craiq7_score = 0,
    popiq7_score = 0
  )

  for (domain in domains) {
    domain_score_name <- paste0(domain, "7_score")
    domain_columns <- paste0(domain, 1:7)

    # Filter only columns that exist in the data
    existing_columns <- domain_columns[domain_columns %in% names(pfiq7_data)]

    if (length(existing_columns) == 0) {
      logger::log_warn("No columns found for domain: {domain}")
      next
    }

    logger::log_debug("Processing {domain} domain with {length(existing_columns)} columns")

    # For each patient, calculate domain score
    for (i in 1:nrow(pfiq7_data)) {
      # Extract patient's domain values
      patient_values <- pfiq7_data[i, existing_columns]

      # Count non-NA values
      non_na_count <- sum(!is.na(patient_values))

      if (non_na_count == 0) {
        logger::log_warn("Patient {i} has no valid responses for {domain} domain")
        domain_results[i, domain_score_name] <- NA
        next
      }

      # Calculate mean of non-NA values, multiply by 7 (to account for all items)
      # then multiply by 100/21 to scale to 0-100
      domain_sum <- sum(patient_values, na.rm = TRUE)
      domain_mean <- domain_sum / non_na_count
      domain_scaled <- domain_mean * 7 * (100/21)

      logger::log_debug("Patient {i} {domain} calculation: sum={domain_sum}, non-NA values={non_na_count}, scaled score={domain_scaled}")

      domain_results[i, domain_score_name] <- domain_scaled
    }
  }

  return(domain_results)
}

#' @noRd
calculate_total_score <- function(domain_scores) {
  logger::log_debug("Calculating total PFIQ-7 score")

  pfiq7_scores <- domain_scores %>%
    dplyr::mutate(
      pfiq7_total = rowSums(
        dplyr::select(., uiq7_score, craiq7_score, popiq7_score),
        na.rm = FALSE
      )
    )

  logger::log_debug("Summary of scores:")
  logger::log_debug("UIQ-7 mean: {mean(pfiq7_scores$uiq7_score, na.rm = TRUE)}")
  logger::log_debug("CRAIQ-7 mean: {mean(pfiq7_scores$craiq7_score, na.rm = TRUE)}")
  logger::log_debug("POPIQ-7 mean: {mean(pfiq7_scores$popiq7_score, na.rm = TRUE)}")
  logger::log_debug("PFIQ-7 total mean: {mean(pfiq7_scores$pfiq7_total, na.rm = TRUE)}")

  return(pfiq7_scores)
}

