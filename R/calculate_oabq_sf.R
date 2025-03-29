utils::globalVariables(c("symptom_score", "hrql_score", "total_score",
                         "symptom_missing", "hrql_missing", "symptom_valid",
                         "hrql_valid", "total_valid"))

#' Calculate OAB-q SF (Overactive Bladder Questionnaire Short Form) Scores
#'
#' @description
#' Calculates scores for the Overactive Bladder Questionnaire Short Form (OAB-q SF),
#' a validated instrument measuring symptom bother and health-related quality of life
#' in patients with overactive bladder. The function includes robust validation,
#' handles missing data according to scoring guidelines, and includes detailed logging.
#'
#' @param questionnaire_data A data frame containing OAB-q SF item responses. Column
#'   names should follow the pattern specified by the `item_prefix` parameter followed
#'   by the item number (e.g., "oab_1", "oab_2", etc.).
#' @param id_column Character string specifying the column name containing patient
#'   identifiers. If NULL (default), no ID column is used.
#' @param item_prefix A character string specifying the prefix for OAB-q SF column names.
#'   Default is "oab_".
#' @param symptom_items A numeric vector specifying which items belong to the symptom
#'   bother scale. Default is items 1-6.
#' @param hrql_items A numeric vector specifying which items belong to the health-related
#'   quality of life scale. Default is items 7-19.
#' @param symptom_missing_threshold Numeric value between 0 and 1 specifying the maximum
#'   proportion of missing values allowed for the symptom scale. Default is 0.5 (50%).
#' @param hrql_missing_threshold Numeric value between 0 and 1 specifying the maximum
#'   proportion of missing values allowed for the HRQL scale. Default is 0.5 (50%).
#' @param verbose Logical indicating whether to display detailed logging information.
#'   Default is FALSE.
#'
#' @return A data frame containing the original data plus additional columns:
#'   \itemize{
#'     \item \strong{symptom_score}: The OAB-q SF Symptom Bother scale score,
#'           transformed to a 0-100 scale where higher scores indicate greater
#'           symptom bother.
#'     \item \strong{hrql_score}: The OAB-q SF Health-Related Quality of Life scale
#'           score, transformed to a 0-100 scale where higher scores indicate better
#'           quality of life.
#'     \item \strong{symptom_missing}: Count of missing items in the Symptom Bother scale.
#'     \item \strong{hrql_missing}: Count of missing items in the HRQL scale.
#'     \item \strong{symptom_valid}: Logical indicating if the Symptom Bother score is
#'           valid (TRUE if missing proportion <= symptom_missing_threshold).
#'     \item \strong{hrql_valid}: Logical indicating if the HRQL score is valid
#'           (TRUE if missing proportion <= hrql_missing_threshold).
#'     \item \strong{total_valid}: Logical indicating if both scores are valid.
#'   }
#'
#' @details
#' The OAB-q SF consists of 19 items across two scales:
#' \itemize{
#'   \item \strong{Symptom Bother} (6 items): Measures the degree of bother associated
#'         with overactive bladder symptoms.
#'   \item \strong{Health-Related Quality of Life} (13 items): Measures the impact
#'         of overactive bladder on various aspects of quality of life.
#' }
#'
#' Each item is scored from 1-6, with higher scores indicating greater bother for the
#' Symptom scale and better quality of life for the HRQL scale.
#'
#' Missing data is handled according to established guidelines:
#' \itemize{
#'   \item If more than 50% of items (or a custom threshold) are missing in a scale,
#'         that scale's score is set to NA.
#'   \item If fewer items are missing, the mean of completed items is used to impute
#'         missing values.
#' }
#'
#' Scores are transformed to a 0-100 scale using the formula:
#' \deqn{Transformed Score = \frac{(Raw Score - Min) \times 100}{(Max - Min)}}
#'
#' @examples
#' # Example 1: Basic usage with minimal data
#' oab_data <- data.frame(
#'   patient_id = c("P001", "P002", "P003"),
#'   oab_1 = c(2, 4, 3),  # Symptom items (1-6)
#'   oab_2 = c(3, 5, 2),
#'   oab_3 = c(4, 3, 1),
#'   oab_4 = c(2, 4, 3),
#'   oab_5 = c(3, 2, 4),
#'   oab_6 = c(1, 3, 2),
#'   oab_7 = c(4, 2, 5),  # HRQL items (7-19)
#'   oab_8 = c(5, 3, 4),
#'   oab_9 = c(3, 4, 5),
#'   oab_10 = c(4, 3, 4),
#'   oab_11 = c(5, 2, 3),
#'   oab_12 = c(4, 3, 5),
#'   oab_13 = c(3, 4, 4),
#'   oab_14 = c(5, 3, 3),
#'   oab_15 = c(4, 5, 4),
#'   oab_16 = c(3, 4, 5),
#'   oab_17 = c(4, 3, 4),
#'   oab_18 = c(5, 4, 3),
#'   oab_19 = c(4, 5, 4)
#' )
#'
#' oab_results <- calculate_oabq_sf(
#'   questionnaire_data = oab_data,
#'   id_column = "patient_id",
#'   verbose = TRUE
#' )
#' print(oab_results)
#'
#' # Example 2: With data containing missing values
#' oab_data_missing <- data.frame(
#'   patient_id = c("P001", "P002", "P003"),
#'   oab_1 = c(2, 4, NA),
#'   oab_2 = c(3, 5, 2),
#'   oab_3 = c(4, NA, 1),
#'   oab_4 = c(2, 4, 3),
#'   oab_5 = c(3, 2, 4),
#'   oab_6 = c(1, 3, 2),
#'   oab_7 = c(4, 2, 5),
#'   oab_8 = c(5, 3, 4),
#'   oab_9 = c(3, NA, 5),
#'   oab_10 = c(4, 3, 4),
#'   oab_11 = c(5, 2, NA),
#'   oab_12 = c(4, 3, 5),
#'   oab_13 = c(3, 4, 4),
#'   oab_14 = c(5, NA, 3),
#'   oab_15 = c(4, 5, 4),
#'   oab_16 = c(3, 4, 5),
#'   oab_17 = c(4, 3, 4),
#'   oab_18 = c(5, 4, NA),
#'   oab_19 = c(4, 5, 4)
#' )
#'
#' oab_results_missing <- calculate_oabq_sf(
#'   questionnaire_data = oab_data_missing,
#'   id_column = "patient_id",
#'   symptom_missing_threshold = 0.3,
#'   hrql_missing_threshold = 0.3,
#'   verbose = TRUE
#' )
#' print(oab_results_missing)
#'
#' # Example 3: With alternative column naming convention
#' alt_oab_data <- data.frame(
#'   record_id = c("R1", "R2", "R3"),
#'   oabq_item_1 = c(2, 4, 3),
#'   oabq_item_2 = c(3, 5, 2),
#'   oabq_item_3 = c(4, 3, 1),
#'   oabq_item_4 = c(2, 4, 3),
#'   oabq_item_5 = c(3, 2, 4),
#'   oabq_item_6 = c(1, 3, 2),
#'   oabq_item_7 = c(4, 2, 5),
#'   oabq_item_8 = c(5, 3, 4),
#'   oabq_item_9 = c(3, 4, 5),
#'   oabq_item_10 = c(4, 3, 4),
#'   oabq_item_11 = c(5, 2, 3),
#'   oabq_item_12 = c(4, 3, 5),
#'   oabq_item_13 = c(3, 4, 4),
#'   oabq_item_14 = c(5, 3, 3),
#'   oabq_item_15 = c(4, 5, 4),
#'   oabq_item_16 = c(3, 4, 5),
#'   oabq_item_17 = c(4, 3, 4),
#'   oabq_item_18 = c(5, 4, 3),
#'   oabq_item_19 = c(4, 5, 4)
#' )
#'
#' oab_results_alt <- calculate_oabq_sf(
#'   questionnaire_data = alt_oab_data,
#'   id_column = "record_id",
#'   item_prefix = "oabq_item_",
#'   verbose = TRUE
#' )
#' print(oab_results_alt)
#'
#' @references
#' Coyne, K. S., Thompson, C. L., Lai, J. S., & Sexton, C. C. (2015).
#' An overactive bladder symptom and health-related quality of life short-form:
#' validation of the OAB-q SF. \emph{Neurourology and Urodynamics, 34}(3), 255-263.
#'
#' @importFrom logger log_info log_debug log_warn log_error log_threshold
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate select bind_cols
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble tibble
#' @importFrom rlang .data
#'
#' @export
calculate_oabq_sf <- function(questionnaire_data,
                              id_column = NULL,
                              item_prefix = "oab_",
                              symptom_items = 1:6,
                              hrql_items = 7:19,
                              symptom_missing_threshold = 0.5,
                              hrql_missing_threshold = 0.5,
                              verbose = FALSE) {
  # Initialize logger
  setup_oabq_logger(verbose)

  # Log function call
  logger::log_info("Starting OAB-q SF calculation")
  logger::log_debug("Input parameters: id_column = %s, item_prefix = %s",
                    ifelse(is.null(id_column), "NULL", id_column), item_prefix)
  logger::log_debug("Symptom items: %s", paste(symptom_items, collapse = ", "))
  logger::log_debug("HRQL items: %s", paste(hrql_items, collapse = ", "))
  logger::log_debug("Missing thresholds: symptom = %.2f, hrql = %.2f",
                    symptom_missing_threshold, hrql_missing_threshold)

  # Validate input data
  validate_oabq_data(questionnaire_data, id_column,
                     symptom_missing_threshold, hrql_missing_threshold)

  # Extract ID column if specified
  id_data <- extract_id_column(questionnaire_data, id_column)

  # Identify OAB-q SF columns
  all_oab_columns <- identify_oabq_columns(questionnaire_data, item_prefix)

  # Extract item numbers and separate by scale
  item_info <- extract_oabq_item_info(all_oab_columns, item_prefix,
                                      symptom_items, hrql_items)

  # Extract and validate OAB-q SF data
  oabq_data <- extract_oabq_data(questionnaire_data, item_info)

  # Calculate symptom bother score
  symptom_results <- calculate_symptom_score(oabq_data, item_info$symptom_columns,
                                             symptom_missing_threshold)

  # Calculate HRQL score
  hrql_results <- calculate_hrql_score(oabq_data, item_info$hrql_columns,
                                       hrql_missing_threshold)

  # Combine results
  score_data <- combine_oabq_scores(symptom_results, hrql_results)

  # Create final result
  final_result <- combine_oabq_results(questionnaire_data, id_data, score_data)

  logger::log_info("OAB-q SF calculation completed successfully")
  logger::log_debug("Final result dimensions: %d rows, %d columns",
                    nrow(final_result), ncol(final_result))

  return(final_result)
}

#' Set up logger for OAB-q SF calculation
#'
#' @param verbose Logical indicating whether to display detailed logging
#' @return NULL invisibly
#' @noRd
setup_oabq_logger <- function(verbose) {
  if (verbose) {
    logger::log_threshold(logger::DEBUG)
    logger::log_debug("Verbose logging enabled")
  } else {
    logger::log_threshold(logger::INFO)
  }

  return(invisible(NULL))
}

#' Validate input data for OAB-q SF calculation
#'
#' @param questionnaire_data Data frame containing questionnaire responses
#' @param id_column Column name for ID
#' @param symptom_missing_threshold Maximum allowed missing proportion for symptom scale
#' @param hrql_missing_threshold Maximum allowed missing proportion for HRQL scale
#' @return NULL invisibly
#' @noRd
validate_oabq_data <- function(questionnaire_data, id_column,
                               symptom_missing_threshold, hrql_missing_threshold) {
  logger::log_debug("Validating input data")

  # Check if input is a data frame
  assertthat::assert_that(is.data.frame(questionnaire_data),
                          msg = "questionnaire_data must be a data frame")

  # Check if data frame has rows
  assertthat::assert_that(nrow(questionnaire_data) > 0,
                          msg = "questionnaire_data must have at least one row")

  # Check if id_column exists if specified
  if (!is.null(id_column)) {
    assertthat::assert_that(id_column %in% names(questionnaire_data),
                            msg = paste0("id_column '", id_column,
                                         "' not found in questionnaire_data"))
  }

  # Validate missing thresholds
  assertthat::assert_that(is.numeric(symptom_missing_threshold) &&
                            symptom_missing_threshold >= 0 &&
                            symptom_missing_threshold <= 1,
                          msg = "symptom_missing_threshold must be a value between 0 and 1")

  assertthat::assert_that(is.numeric(hrql_missing_threshold) &&
                            hrql_missing_threshold >= 0 &&
                            hrql_missing_threshold <= 1,
                          msg = "hrql_missing_threshold must be a value between 0 and 1")

  logger::log_debug("Input validation completed")
  return(invisible(NULL))
}

#' Extract ID column from questionnaire data
#'
#' @param questionnaire_data Data frame containing questionnaire responses
#' @param id_column Column name for ID
#' @return Data frame with ID column or NULL
#' @noRd
extract_id_column <- function(questionnaire_data, id_column) {
  if (!is.null(id_column)) {
    logger::log_debug("Extracting ID column: %s", id_column)
    return(questionnaire_data[, id_column, drop = FALSE])
  } else {
    logger::log_debug("No ID column specified")
    return(NULL)
  }
}

#' Identify OAB-q SF columns in data frame
#'
#' @param questionnaire_data Data frame containing questionnaire responses
#' @param item_prefix Prefix for OAB-q SF item columns
#' @return Character vector of column names
#' @noRd
identify_oabq_columns <- function(questionnaire_data, item_prefix) {
  logger::log_debug("Identifying OAB-q SF columns with prefix: %s", item_prefix)

  # Create pattern for OAB-q SF columns
  oabq_pattern <- paste0("^", item_prefix, "(\\d+)$")

  # Find matching columns
  oabq_columns <- grep(oabq_pattern, names(questionnaire_data), value = TRUE)

  # Check if any columns were found
  if (length(oabq_columns) == 0) {
    logger::log_error("No OAB-q SF item columns found with prefix: %s", item_prefix)
    stop("No OAB-q SF item columns found with prefix: ", item_prefix)
  }

  logger::log_debug("Found %d OAB-q SF columns", length(oabq_columns))
  return(oabq_columns)
}

#' Extract OAB-q SF item information and organize by scale
#'
#' @param all_oab_columns Character vector of all OAB-q SF column names
#' @param item_prefix Prefix for OAB-q SF item columns
#' @param symptom_items Numeric vector of symptom scale item numbers
#' @param hrql_items Numeric vector of HRQL scale item numbers
#' @return List with symptom and HRQL column names and expected counts
#' @noRd
extract_oabq_item_info <- function(all_oab_columns, item_prefix,
                                   symptom_items, hrql_items) {
  logger::log_debug("Extracting OAB-q SF item information and organizing by scale")

  # Extract item numbers from column names
  item_numbers <- as.numeric(gsub(paste0("^", item_prefix, "(\\d+)$"), "\\1",
                                  all_oab_columns))

  # Create column names by scale
  symptom_columns <- all_oab_columns[item_numbers %in% symptom_items]
  hrql_columns <- all_oab_columns[item_numbers %in% hrql_items]

  # Log what was found
  logger::log_debug("Found %d symptom scale columns (expected %d)",
                    length(symptom_columns), length(symptom_items))
  logger::log_debug("Found %d HRQL scale columns (expected %d)",
                    length(hrql_columns), length(hrql_items))

  # Warn if not all expected columns were found
  if (length(symptom_columns) < length(symptom_items)) {
    missing_symptom_items <- symptom_items[!symptom_items %in% item_numbers]
    logger::log_warn("Missing symptom scale items: %s",
                     paste(missing_symptom_items, collapse = ", "))
  }

  if (length(hrql_columns) < length(hrql_items)) {
    missing_hrql_items <- hrql_items[!hrql_items %in% item_numbers]
    logger::log_warn("Missing HRQL scale items: %s",
                     paste(missing_hrql_items, collapse = ", "))
  }

  # Return organized information
  return(list(
    symptom_columns = symptom_columns,
    hrql_columns = hrql_columns,
    expected_symptom_count = length(symptom_items),
    expected_hrql_count = length(hrql_items)
  ))
}

#' Extract and validate OAB-q SF data
#'
#' @param questionnaire_data Data frame containing questionnaire responses
#' @param item_info List with symptom and HRQL column information
#' @return Data frame with validated OAB-q SF data
#' @noRd
extract_oabq_data <- function(questionnaire_data, item_info) {
  logger::log_debug("Extracting and validating OAB-q SF data")

  # Combine all OAB-q SF columns
  all_columns <- c(item_info$symptom_columns, item_info$hrql_columns)

  # Extract data
  oabq_data <- questionnaire_data[, all_columns, drop = FALSE]

  # Process each column to ensure it's numeric and in valid range
  for (col in names(oabq_data)) {
    # Convert to numeric if not already
    if (!is.numeric(oabq_data[[col]])) {
      logger::log_warn("Converting column %s to numeric", col)
      oabq_data[[col]] <- as.numeric(oabq_data[[col]])
    }

    # Check for values outside valid range (1-6)
    valid_range <- oabq_data[[col]] >= 1 & oabq_data[[col]] <= 6 | is.na(oabq_data[[col]])

    if (!all(valid_range)) {
      invalid_indices <- which(!valid_range)
      invalid_values <- oabq_data[[col]][invalid_indices]

      logger::log_warn("Column %s contains %d values outside valid range (1-6)",
                       col, length(invalid_indices))

      # Clip values to valid range
      oabq_data[[col]][!is.na(oabq_data[[col]]) & oabq_data[[col]] < 1] <- 1
      oabq_data[[col]][!is.na(oabq_data[[col]]) & oabq_data[[col]] > 6] <- 6

      logger::log_warn("Invalid values in column %s have been clipped to range 1-6", col)
    }
  }

  logger::log_debug("OAB-q SF data extraction and validation completed")
  return(oabq_data)
}

#' Calculate Symptom Bother scale score
#'
#' @param oabq_data Data frame with validated OAB-q SF data
#' @param symptom_columns Character vector of symptom scale column names
#' @param missing_threshold Maximum allowed missing proportion
#' @return Data frame with symptom scale results
#' @noRd
calculate_symptom_score <- function(oabq_data, symptom_columns, missing_threshold) {
  logger::log_debug("Calculating Symptom Bother scale score")

  # Extract symptom data
  symptom_data <- oabq_data[, symptom_columns, drop = FALSE]

  # Count missing values
  missing_count <- rowSums(is.na(symptom_data))
  total_items <- length(symptom_columns)
  missing_proportion <- missing_count / total_items

  # Calculate minimum required responses
  min_required <- ceiling(total_items * (1 - missing_threshold))
  logger::log_debug("Minimum required responses for symptom scale: %d out of %d",
                    min_required, total_items)

  # Calculate valid rows (those with sufficient responses)
  valid_rows <- missing_count <= (total_items - min_required)
  logger::log_debug("%d out of %d rows have valid symptom data",
                    sum(valid_rows), length(valid_rows))

  # Calculate raw scores for valid rows
  raw_scores <- rep(NA_real_, nrow(oabq_data))

  if (any(valid_rows)) {
    # For valid rows, calculate mean of non-missing items
    means <- rowMeans(symptom_data[valid_rows, , drop = FALSE], na.rm = TRUE)

    # Transform to 0-100 scale
    # Formula: ((mean - 1) / 5) * 100
    transformed_scores <- ((means - 1) / 5) * 100

    # Assign to valid rows
    raw_scores[valid_rows] <- transformed_scores

    logger::log_debug("Symptom Bother scores range from %.1f to %.1f",
                      min(transformed_scores, na.rm = TRUE),
                      max(transformed_scores, na.rm = TRUE))
  }

  # Create results data frame
  symptom_results <- data.frame(
    symptom_score = raw_scores,
    symptom_missing = missing_count,
    symptom_valid = valid_rows
  )

  logger::log_debug("Symptom Bother scale calculation completed")
  return(symptom_results)
}

#' Calculate Health-Related Quality of Life scale score
#'
#' @param oabq_data Data frame with validated OAB-q SF data
#' @param hrql_columns Character vector of HRQL scale column names
#' @param missing_threshold Maximum allowed missing proportion
#' @return Data frame with HRQL scale results
#' @noRd
calculate_hrql_score <- function(oabq_data, hrql_columns, missing_threshold) {
  logger::log_debug("Calculating Health-Related Quality of Life scale score")

  # Extract HRQL data
  hrql_data <- oabq_data[, hrql_columns, drop = FALSE]

  # Count missing values
  missing_count <- rowSums(is.na(hrql_data))
  total_items <- length(hrql_columns)
  missing_proportion <- missing_count / total_items

  # Calculate minimum required responses
  min_required <- ceiling(total_items * (1 - missing_threshold))
  logger::log_debug("Minimum required responses for HRQL scale: %d out of %d",
                    min_required, total_items)

  # Calculate valid rows (those with sufficient responses)
  valid_rows <- missing_count <= (total_items - min_required)
  logger::log_debug("%d out of %d rows have valid HRQL data",
                    sum(valid_rows), length(valid_rows))

  # Calculate raw scores for valid rows
  raw_scores <- rep(NA_real_, nrow(oabq_data))

  if (any(valid_rows)) {
    # For valid rows, calculate mean of non-missing items
    means <- rowMeans(hrql_data[valid_rows, , drop = FALSE], na.rm = TRUE)

    # Transform to 0-100 scale
    # Formula: ((mean - 1) / 5) * 100
    transformed_scores <- ((means - 1) / 5) * 100

    # Assign to valid rows
    raw_scores[valid_rows] <- transformed_scores

    logger::log_debug("HRQL scores range from %.1f to %.1f",
                      min(transformed_scores, na.rm = TRUE),
                      max(transformed_scores, na.rm = TRUE))
  }

  # Create results data frame
  hrql_results <- data.frame(
    hrql_score = raw_scores,
    hrql_missing = missing_count,
    hrql_valid = valid_rows
  )

  logger::log_debug("HRQL scale calculation completed")
  return(hrql_results)
}

#' Combine symptom and HRQL scores
#'
#' @param symptom_results Data frame with symptom scale results
#' @param hrql_results Data frame with HRQL scale results
#' @return Data frame with combined scores
#' @noRd
combine_oabq_scores <- function(symptom_results, hrql_results) {
  logger::log_debug("Combining symptom and HRQL scores")

  # Combine symptom and HRQL data
  combined_data <- cbind(symptom_results, hrql_results)

  # Add total validity flag
  combined_data$total_valid <- combined_data$symptom_valid & combined_data$hrql_valid

  logger::log_debug("%d out of %d rows have both valid symptom and HRQL scores",
                    sum(combined_data$total_valid), nrow(combined_data))

  return(combined_data)
}

#' Combine all results into final output
#'
#' @param questionnaire_data Original questionnaire data
#' @param id_data Data frame with ID column or NULL
#' @param score_data Data frame with calculated scores
#' @return Combined data frame with all data
#' @noRd
combine_oabq_results <- function(questionnaire_data, id_data, score_data) {
  logger::log_debug("Combining results into final output")

  if (is.null(id_data)) {
    # If no ID column was specified, just add score data to original data
    final_result <- cbind(questionnaire_data, score_data)
  } else {
    # If ID column was specified, construct result
    final_result <- cbind(questionnaire_data, score_data)
  }

  return(final_result)
}
