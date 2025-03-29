utils::globalVariables(c("pisq12_score", "pisq12_missing", "pisq12_valid"))

#' Calculate PISQ-12 (Pelvic Organ Prolapse/Urinary Incontinence Sexual Questionnaire) Scores
#'
#' @description
#' Calculates the Pelvic Organ Prolapse/Urinary Incontinence Sexual Questionnaire
#' (PISQ-12) score, which measures sexual function in women with pelvic floor
#' disorders. The function handles missing data, performs reverse scoring on appropriate
#' items, and validates inputs according to scoring guidelines.
#'
#' @param questionnaire_data A data frame containing PISQ-12 item responses. Column
#'   names should follow the pattern specified by the `item_prefix` parameter followed
#'   by the item number (e.g., "pisq_1", "pisq_2", etc.).
#' @param id_column Character string specifying the column name containing patient
#'   identifiers. If NULL (default), no ID column is used.
#' @param item_prefix A character string specifying the prefix for PISQ-12 column names.
#'   Default is "pisq_".
#' @param reverse_items A numeric vector specifying which items should be reverse-scored.
#'   Default is items 1, 2, 3, and 4, which are positively phrased items.
#' @param max_missing The maximum number of missing items allowed before score is set
#'   to NA. Default is 2 (up to 2 missing items permitted).
#' @param scale_to_100 Logical indicating whether to scale scores to a 0-100 range.
#'   Default is FALSE (returns raw scores from 0-48).
#' @param verbose Logical indicating whether to display detailed logging information.
#'   Default is FALSE.
#'
#' @return A data frame containing the original data plus additional columns:
#'   \itemize{
#'     \item \strong{pisq12_score}: The PISQ-12 score. If \code{scale_to_100 = TRUE},
#'           scores range from 0-100, with higher scores indicating better sexual
#'           function. Otherwise, scores range from 0-48.
#'     \item \strong{pisq12_missing}: Count of missing items for each respondent.
#'     \item \strong{pisq12_valid}: Logical indicating if the score is valid (TRUE if
#'           missing items <= max_missing).
#'   }
#'
#' @details
#' The PISQ-12 consists of 12 items measuring sexual function across three domains:
#' behavioral/emotive (items 1-4), physical (items 5-9), and partner-related (items 10-12).
#' Each item is scored from 0 (always) to 4 (never).
#'
#' Items 1-4 ask about positive aspects of sexual function and are reverse-scored so that
#' higher scores indicate better function. For all items, after any necessary reverse-scoring,
#' higher scores indicate better sexual function.
#'
#' Missing data is handled by:
#' 1. Calculating the mean of answered items
#' 2. Substituting this mean for missing values
#' 3. Multiplying by 12/(12-n), where n is the number of missing items
#' 4. If more than `max_missing` items are missing, the score is set to NA
#'
#' @examples
#' # Example 1: Basic usage with minimal data
#' pisq_data <- data.frame(
#'   patient_id = c("P001", "P002", "P003"),
#'   pisq_1 = c(3, 2, 1),  # Items 1-4 are positively phrased (reverse-scored)
#'   pisq_2 = c(4, 3, 2),
#'   pisq_3 = c(2, 3, 4),
#'   pisq_4 = c(3, 2, 1),
#'   pisq_5 = c(0, 1, 2),  # Items 5-12 are negatively phrased
#'   pisq_6 = c(1, 0, 3),
#'   pisq_7 = c(2, 1, 0),
#'   pisq_8 = c(1, 2, 1),
#'   pisq_9 = c(0, 1, 2),
#'   pisq_10 = c(1, 0, 3),
#'   pisq_11 = c(2, 1, 0),
#'   pisq_12 = c(1, 0, 1)
#' )
#'
#' pisq12_results <- calculate_pisq12(
#'   questionnaire_data = pisq_data,
#'   id_column = "patient_id",
#'   scale_to_100 = FALSE,
#'   verbose = TRUE
#' )
#' print(pisq12_results)
#'
#' # Example 2: With data containing missing values
#' pisq_data_missing <- data.frame(
#'   patient_id = c("P001", "P002", "P003"),
#'   pisq_1 = c(3, 2, NA),
#'   pisq_2 = c(4, 3, 2),
#'   pisq_3 = c(2, NA, 4),
#'   pisq_4 = c(3, 2, 1),
#'   pisq_5 = c(0, 1, 2),
#'   pisq_6 = c(1, 0, 3),
#'   pisq_7 = c(2, 1, NA),
#'   pisq_8 = c(1, 2, 1),
#'   pisq_9 = c(0, 1, 2),
#'   pisq_10 = c(1, NA, 3),
#'   pisq_11 = c(2, 1, 0),
#'   pisq_12 = c(1, 0, 1)
#' )
#'
#' pisq12_results_missing <- calculate_pisq12(
#'   questionnaire_data = pisq_data_missing,
#'   id_column = "patient_id",
#'   max_missing = 2,
#'   scale_to_100 = TRUE,
#'   verbose = TRUE
#' )
#' print(pisq12_results_missing)
#'
#' # Example 3: With alternative column naming convention
#' alt_pisq_data <- data.frame(
#'   record_id = c("R1", "R2", "R3"),
#'   pisq_item_1 = c(3, 2, 1),
#'   pisq_item_2 = c(4, 3, 2),
#'   pisq_item_3 = c(2, 3, 4),
#'   pisq_item_4 = c(3, 2, 1),
#'   pisq_item_5 = c(0, 1, 2),
#'   pisq_item_6 = c(1, 0, 3),
#'   pisq_item_7 = c(2, 1, 0),
#'   pisq_item_8 = c(1, 2, 1),
#'   pisq_item_9 = c(0, 1, 2),
#'   pisq_item_10 = c(1, 0, 3),
#'   pisq_item_11 = c(2, 1, 0),
#'   pisq_item_12 = c(1, 0, 1)
#' )
#'
#' pisq12_results_alt <- calculate_pisq12(
#'   questionnaire_data = alt_pisq_data,
#'   id_column = "record_id",
#'   item_prefix = "pisq_item_",
#'   scale_to_100 = TRUE,
#'   verbose = TRUE
#' )
#' print(pisq12_results_alt)
#'
#' @references
#' Rogers RG, Coates KW, Kammerer-Doak D, Khalsa S, Qualls C (2003).
#' A short form of the Pelvic Organ Prolapse/Urinary Incontinence Sexual
#' Questionnaire (PISQ-12). \emph{International Urogynecology Journal and
#' Pelvic Floor Dysfunction, 14}(3), 164-168.
#'
#' @importFrom logger log_info log_debug log_warn log_error log_threshold
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate select bind_cols
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble tibble
#' @importFrom rlang .data
#'
#' @export
calculate_pisq12 <- function(questionnaire_data,
                             id_column = NULL,
                             item_prefix = "pisq_",
                             reverse_items = c(1, 2, 3, 4),
                             max_missing = 2,
                             scale_to_100 = FALSE,
                             verbose = FALSE) {
  # Initialize logger
  setup_pisq12_logger(verbose)

  # Log function call
  logger::log_info("Starting PISQ-12 calculation")
  logger::log_debug("Input parameters: id_column = %s, item_prefix = %s, max_missing = %d, scale_to_100 = %s",
                    ifelse(is.null(id_column), "NULL", id_column),
                    item_prefix, max_missing, scale_to_100)

  # Validate input data
  validate_pisq12_data(questionnaire_data, id_column, item_prefix, max_missing)

  # Find PISQ-12 columns
  pisq_columns <- identify_pisq12_columns(questionnaire_data, item_prefix)
  logger::log_debug("Identified %d PISQ-12 columns", length(pisq_columns))

  # Extract ID column if specified
  id_data <- extract_id_column(questionnaire_data, id_column)

  # Extract PISQ items data
  pisq_items_data <- questionnaire_data[, pisq_columns, drop = FALSE]
  logger::log_debug("Extracted PISQ-12 item data: %d rows, %d columns",
                    nrow(pisq_items_data), ncol(pisq_items_data))

  # Process data types
  pisq_items_data <- process_pisq12_data_types(pisq_items_data)

  # Apply reverse scoring
  processed_data <- apply_reverse_scoring(pisq_items_data, item_prefix, reverse_items)
  logger::log_debug("Applied reverse scoring to items: %s",
                    paste(reverse_items, collapse = ", "))

  # Count missing values
  missing_count <- rowSums(is.na(processed_data))
  logger::log_debug("Missing value counts range from %d to %d per row",
                    min(missing_count), max(missing_count))

  # Calculate scores
  score_data <- calculate_pisq12_scores(processed_data, missing_count, max_missing, scale_to_100)
  logger::log_info("PISQ-12 calculation completed")

  # Combine all data
  final_result <- combine_pisq12_results(questionnaire_data, id_data, score_data)
  logger::log_debug("Final result dimensions: %d rows, %d columns",
                    nrow(final_result), ncol(final_result))

  return(final_result)
}

#' Set up logger for PISQ-12 calculation
#'
#' @param verbose Logical indicating whether to display detailed logging
#' @return NULL invisibly
#' @noRd
setup_pisq12_logger <- function(verbose) {
  if (verbose) {
    logger::log_threshold(logger::DEBUG)
    logger::log_debug("Verbose logging enabled")
  } else {
    logger::log_threshold(logger::INFO)
  }
}

#' Validate input data for PISQ-12 calculation
#'
#' @param questionnaire_data Data frame containing questionnaire responses
#' @param id_column Column name for ID
#' @param item_prefix Prefix for PISQ-12 item columns
#' @param max_missing Maximum allowed missing items
#' @return NULL invisibly
#' @noRd
validate_pisq12_data <- function(questionnaire_data, id_column, item_prefix, max_missing) {
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
                            msg = paste0("id_column '", id_column, "' not found in questionnaire_data"))
  }

  # Check if max_missing is valid
  assertthat::assert_that(is.numeric(max_missing) && max_missing >= 0 && max_missing <= 11,
                          msg = "max_missing must be a number between 0 and 11")

  logger::log_debug("Input validation completed")
  return(invisible(NULL))
}

#' Identify PISQ-12 columns in data frame
#'
#' @param questionnaire_data Data frame containing questionnaire responses
#' @param item_prefix Prefix for PISQ-12 item columns
#' @return Character vector of column names
#' @noRd
identify_pisq12_columns <- function(questionnaire_data, item_prefix) {
  logger::log_debug("Identifying PISQ-12 columns with prefix: %s", item_prefix)

  # Create pattern for PISQ-12 columns
  pisq_pattern <- paste0("^", item_prefix, "([1-9]|1[0-2])$")

  # Find matching columns
  pisq_columns <- grep(pisq_pattern, names(questionnaire_data), value = TRUE)

  # Check if any columns were found
  if (length(pisq_columns) == 0) {
    logger::log_error("No PISQ-12 item columns found with prefix: %s", item_prefix)
    stop("No PISQ-12 item columns found with prefix: ", item_prefix)
  }

  # Check if all 12 items are present
  if (length(pisq_columns) < 12) {
    logger::log_warn("Found only %d of 12 expected PISQ-12 items", length(pisq_columns))
  }

  return(pisq_columns)
}

#' Extract ID column from data
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

#' Process data types for PISQ-12 items
#'
#' @param pisq_items_data Data frame containing only PISQ-12 items
#' @return Data frame with processed data types
#' @noRd
process_pisq12_data_types <- function(pisq_items_data) {
  logger::log_debug("Processing data types for PISQ-12 items")

  for (col in names(pisq_items_data)) {
    # Convert to numeric if not already
    if (!is.numeric(pisq_items_data[[col]])) {
      logger::log_warn("Converting column %s to numeric", col)
      pisq_items_data[[col]] <- as.numeric(pisq_items_data[[col]])
    }

    # Check for values outside valid range (0-4)
    valid_range <- pisq_items_data[[col]] >= 0 & pisq_items_data[[col]] <= 4 | is.na(pisq_items_data[[col]])

    if (!all(valid_range)) {
      invalid_values <- pisq_items_data[[col]][!valid_range]
      logger::log_warn("Column %s contains values outside valid range (0-4): %s",
                       col, paste(invalid_values, collapse = ", "))

      # Clip values to valid range
      pisq_items_data[[col]][!is.na(pisq_items_data[[col]]) & pisq_items_data[[col]] < 0] <- 0
      pisq_items_data[[col]][!is.na(pisq_items_data[[col]]) & pisq_items_data[[col]] > 4] <- 4

      logger::log_warn("Invalid values in column %s have been clipped to range 0-4", col)
    }
  }

  logger::log_debug("Data type processing completed")
  return(pisq_items_data)
}

#' Apply reverse scoring to specified items
#'
#' @param pisq_items_data Data frame containing only PISQ-12 items
#' @param item_prefix Prefix for PISQ-12 item columns
#' @param reverse_items Numeric vector of items to reverse-score
#' @return Data frame with reverse scoring applied
#' @noRd
apply_reverse_scoring <- function(pisq_items_data, item_prefix, reverse_items) {
  logger::log_debug("Applying reverse scoring to items: %s",
                    paste(reverse_items, collapse = ", "))

  # Make a copy of the data
  processed_data <- pisq_items_data

  # Extract item numbers from column names
  col_numbers <- as.numeric(gsub(paste0("^", item_prefix, "([0-9]+)$"), "\\1",
                                 names(processed_data)))

  # Apply reverse scoring
  for (i in seq_along(processed_data)) {
    item_number <- col_numbers[i]
    if (item_number %in% reverse_items) {
      logger::log_debug("Reverse scoring item %d", item_number)
      processed_data[[i]] <- 4 - processed_data[[i]]
    }
  }

  return(processed_data)
}

#' Calculate PISQ-12 scores
#'
#' @param processed_data Data frame with processed and reverse-scored PISQ-12 items
#' @param missing_count Vector of missing item counts per row
#' @param max_missing Maximum allowed missing items
#' @param scale_to_100 Whether to scale scores to 0-100 range
#' @return Data frame with PISQ-12 scores and related metrics
#' @noRd
calculate_pisq12_scores <- function(processed_data, missing_count, max_missing, scale_to_100) {
  logger::log_debug("Calculating PISQ-12 scores")

  # Calculate mean for each respondent (for imputation)
  row_means <- rowMeans(processed_data, na.rm = TRUE)

  # Replace missing values with mean
  imputed_data <- processed_data
  for (i in 1:nrow(processed_data)) {
    if (missing_count[i] > 0 && missing_count[i] <= max_missing) {
      logger::log_debug("Imputing %d missing values for row %d with mean %.2f",
                        missing_count[i], i, row_means[i])

      for (j in 1:ncol(processed_data)) {
        if (is.na(imputed_data[i, j])) {
          imputed_data[i, j] <- row_means[i]
        }
      }
    }
  }

  # Calculate raw total score
  raw_scores <- rowSums(imputed_data)

  # Apply correction for missing items
  scaling_factor <- 12 / (12 - pmin(missing_count, max_missing))
  corrected_scores <- raw_scores * scaling_factor

  # Set scores to NA if too many missing items
  corrected_scores[missing_count > max_missing] <- NA

  # Scale to 0-100 if requested
  final_scores <- corrected_scores
  if (scale_to_100) {
    logger::log_debug("Scaling scores to 0-100 range")
    final_scores <- (corrected_scores / 48) * 100
  }

  # Valid scores flag
  valid_scores <- missing_count <= max_missing

  # Create results data frame
  score_data <- data.frame(
    pisq12_score = final_scores,
    pisq12_missing = missing_count,
    pisq12_valid = valid_scores
  )

  logger::log_debug("Score calculation completed")
  return(score_data)
}

#' Combine all results into final output
#'
#' @param questionnaire_data Original questionnaire data
#' @param id_data Data frame with ID column or NULL
#' @param score_data Data frame with calculated scores
#' @return Combined data frame with all data
#' @noRd
combine_pisq12_results <- function(questionnaire_data, id_data, score_data) {
  logger::log_debug("Combining results into final output")

  if (is.null(id_data)) {
    # If no ID column was specified, just add score data to original data
    final_result <- cbind(questionnaire_data, score_data)
  } else {
    # If ID column was specified, construct result with ID first
    final_result <- cbind(id_data, score_data)
  }

  return(final_result)
}
