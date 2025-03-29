utils::globalVariables(c("iciq_score", "frequency_score", "volume_score", "impact_score",
                         "iciq_missing", "iciq_valid", "situation_data"))

#' Calculate ICIQ-UI SF (International Consultation on Incontinence Questionnaire-Urinary
#' Incontinence Short Form) Scores
#'
#' @description
#' Calculates scores for the ICIQ-UI SF, a validated instrument measuring urinary
#' incontinence symptoms and their impact on quality of life. The function includes
#' robust validation, handles missing data according to scoring guidelines, and provides
#' detailed analysis of incontinence situations.
#'
#' @param questionnaire_data A data frame containing ICIQ-UI SF item responses. Column
#'   names should follow the pattern specified by the parameters or default structure.
#' @param id_column Character string specifying the column name containing patient
#'   identifiers. If NULL (default), no ID column is used.
#' @param frequency_item Character string specifying the column name containing the
#'   frequency question (item 1). Default is "iciq_1".
#' @param volume_item Character string specifying the column name containing the
#'   leakage amount question (item 2). Default is "iciq_2".
#' @param impact_item Character string specifying the column name containing the
#'   quality of life impact question (item 3). Default is "iciq_3".
#' @param situation_prefix Character string specifying the prefix for situation
#'   columns (item 4). Default is "iciq_4_".
#' @param calculate_situations Logical indicating whether to analyze incontinence
#'   situations. Default is TRUE.
#' @param missing_threshold Numeric value between 0 and 1 specifying the maximum
#'   proportion of missing values allowed for the core items (1-3). Default is 0.0 (no
#'   missing values allowed).
#' @param verbose Logical indicating whether to display detailed logging information.
#'   Default is FALSE.
#'
#' @return A data frame containing the original data plus additional columns:
#'   \itemize{
#'     \item \strong{iciq_score}: The ICIQ-UI SF total score (sum of items 1-3),
#'           ranging from 0-21, with higher scores indicating greater severity.
#'     \item \strong{frequency_score}: Score for urinary frequency (item 1, 0-5).
#'     \item \strong{volume_score}: Score for leakage amount (item 2, 0-6).
#'     \item \strong{impact_score}: Score for quality of life impact (item 3, 0-10).
#'     \item \strong{iciq_missing}: Count of missing core items (1-3).
#'     \item \strong{iciq_valid}: Logical indicating if the score is valid.
#'     \item \strong{situation_*}: If calculate_situations=TRUE, columns for each
#'           incontinence situation.
#'   }
#'
#' @details
#' The ICIQ-UI SF consists of 4 items:
#' \itemize{
#'   \item \strong{Item 1}: Frequency of urinary leakage (scored 0-5)
#'   \item \strong{Item 2}: Amount of leakage (scored 0-6)
#'   \item \strong{Item 3}: Impact on quality of life (scored 0-10)
#'   \item \strong{Item 4}: Situations when leakage occurs (not scored, diagnostic only)
#' }
#'
#' The total ICIQ-UI SF score is the sum of items 1-3, ranging from 0-21:
#' \itemize{
#'   \item 1-5: Mild urinary incontinence
#'   \item 6-12: Moderate urinary incontinence
#'   \item 13-21: Severe urinary incontinence
#' }
#'
#' Missing data handling:
#' \itemize{
#'   \item By default, if any items 1-3 are missing, the score is set to NA
#'   \item This can be customized with the missing_threshold parameter
#'   \item Missing situation items (item 4) are treated as "not selected"
#' }
#'
#' @examples
#' # Example 1: Basic usage with minimal data
#' iciq_data <- data.frame(
#'   patient_id = c("P001", "P002", "P003"),
#'   iciq_1 = c(2, 3, 1),     # Frequency (0-5)
#'   iciq_2 = c(4, 2, 0),     # Volume (0-6)
#'   iciq_3 = c(6, 8, 3),     # Impact (0-10)
#'   iciq_4_1 = c(1, 0, 0),   # Situation: leaks before reaching toilet
#'   iciq_4_2 = c(0, 1, 0),   # Situation: leaks when coughing/sneezing
#'   iciq_4_3 = c(0, 0, 1),   # Situation: leaks when asleep
#'   iciq_4_4 = c(0, 1, 0),   # Situation: leaks with physical activity
#'   iciq_4_5 = c(1, 0, 0),   # Situation: leaks when finished urinating
#'   iciq_4_6 = c(0, 0, 0),   # Situation: leaks for no obvious reason
#'   iciq_4_7 = c(0, 0, 0)    # Situation: leaks all the time
#' )
#'
#' iciq_results <- calculate_iciq_ui_sf(
#'   questionnaire_data = iciq_data,
#'   id_column = "patient_id",
#'   verbose = TRUE
#' )
#' print(iciq_results)
#'
#' # Example 2: With data containing missing values
#' iciq_data_missing <- data.frame(
#'   patient_id = c("P001", "P002", "P003"),
#'   iciq_1 = c(2, NA, 1),    # Frequency (0-5)
#'   iciq_2 = c(4, 2, 0),     # Volume (0-6)
#'   iciq_3 = c(6, 8, NA),    # Impact (0-10)
#'   iciq_4_1 = c(1, 0, 0),   # Situation: leaks before reaching toilet
#'   iciq_4_2 = c(0, 1, 0),   # Situation: leaks when coughing/sneezing
#'   iciq_4_3 = c(0, NA, 1)   # Situation: leaks when asleep
#' )
#'
#' iciq_results_missing <- calculate_iciq_ui_sf(
#'   questionnaire_data = iciq_data_missing,
#'   id_column = "patient_id",
#'   missing_threshold = 0.33,  # Allow up to 33% missing items
#'   verbose = TRUE
#' )
#' print(iciq_results_missing)
#'
#' # Example 3: With alternative column naming convention
#' alt_iciq_data <- data.frame(
#'   record_id = c("R1", "R2", "R3"),
#'   frequency = c(2, 3, 1),      # Frequency (0-5)
#'   amount = c(4, 2, 0),         # Volume (0-6)
#'   qol_impact = c(6, 8, 3),     # Impact (0-10)
#'   sit_toilet = c(1, 0, 0),     # Situation: before reaching toilet
#'   sit_cough = c(0, 1, 0),      # Situation: when coughing/sneezing
#'   sit_sleep = c(0, 0, 1)       # Situation: when asleep
#' )
#'
#' iciq_results_alt <- calculate_iciq_ui_sf(
#'   questionnaire_data = alt_iciq_data,
#'   id_column = "record_id",
#'   frequency_item = "frequency",
#'   volume_item = "amount",
#'   impact_item = "qol_impact",
#'   situation_prefix = "sit_",
#'   verbose = TRUE
#' )
#' print(iciq_results_alt)
#'
#' @references
#' Avery K, Donovan J, Peters TJ, Shaw C, Gotoh M, Abrams P (2004).
#' ICIQ: a brief and robust measure for evaluating the symptoms and impact
#' of urinary incontinence. \emph{Neurourology and Urodynamics, 23}(4), 322-330.
#'
#' @importFrom logger log_info log_debug log_warn log_error log_threshold
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate select bind_cols filter across
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @importFrom purrr map_df
#' @importFrom stringr str_detect
#'
#' @export
calculate_iciq_ui_sf <- function(questionnaire_data,
                                 id_column = NULL,
                                 frequency_item = "iciq_1",
                                 volume_item = "iciq_2",
                                 impact_item = "iciq_3",
                                 situation_prefix = "iciq_4_",
                                 calculate_situations = TRUE,
                                 missing_threshold = 0.0,
                                 verbose = FALSE) {
  # Initialize logger
  setup_iciq_logger(verbose)

  # Log function call
  logger::log_info("Starting ICIQ-UI SF calculation")
  logger::log_debug("Input parameters: id_column = %s",
                    ifelse(is.null(id_column), "NULL", id_column))
  logger::log_debug("Item columns: frequency = %s, volume = %s, impact = %s",
                    frequency_item, volume_item, impact_item)
  logger::log_debug("Situation prefix: %s, calculate_situations = %s",
                    situation_prefix, calculate_situations)
  logger::log_debug("Missing threshold: %.2f", missing_threshold)

  # Validate input data
  validate_iciq_data(questionnaire_data, id_column, frequency_item,
                     volume_item, impact_item, missing_threshold)

  # Extract ID column if specified
  id_data <- extract_id_column(questionnaire_data, id_column)

  # Extract core ICIQ-UI SF items (items 1-3)
  core_items <- c(frequency_item, volume_item, impact_item)
  core_data <- extract_core_iciq_data(questionnaire_data, core_items)

  # Process and validate core item data
  core_data <- process_iciq_data_types(core_data, core_items)

  # Calculate ICIQ-UI SF scores and handle missing data
  score_data <- calculate_iciq_scores(core_data, core_items, missing_threshold)

  # Process situation data if requested
  if (calculate_situations) {
    situation_data <- extract_situation_data(questionnaire_data, situation_prefix)
    score_data <- combine_with_situations(score_data, situation_data)
  }

  # Create final result
  final_result <- combine_iciq_results(questionnaire_data, id_data, score_data)

  logger::log_info("ICIQ-UI SF calculation completed successfully")
  logger::log_debug("Final result dimensions: %d rows, %d columns",
                    nrow(final_result), ncol(final_result))

  return(final_result)
}

#' Set up logger for ICIQ-UI SF calculation
#'
#' @param verbose Logical indicating whether to display detailed logging
#' @return NULL invisibly
#' @noRd
setup_iciq_logger <- function(verbose) {
  if (verbose) {
    logger::log_threshold(logger::DEBUG)
    logger::log_debug("Verbose logging enabled")
  } else {
    logger::log_threshold(logger::INFO)
  }

  return(invisible(NULL))
}

#' Validate input data for ICIQ-UI SF calculation
#'
#' @param questionnaire_data Data frame containing questionnaire responses
#' @param id_column Column name for ID
#' @param frequency_item Column name for frequency item
#' @param volume_item Column name for volume item
#' @param impact_item Column name for impact item
#' @param missing_threshold Maximum allowed missing proportion
#' @return NULL invisibly
#' @noRd
validate_iciq_data <- function(questionnaire_data, id_column,
                               frequency_item, volume_item, impact_item,
                               missing_threshold) {
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

  # Check if core item columns exist
  core_items <- c(frequency_item, volume_item, impact_item)
  missing_items <- core_items[!core_items %in% names(questionnaire_data)]

  if (length(missing_items) > 0) {
    logger::log_error("Missing required ICIQ-UI SF columns: %s",
                      paste(missing_items, collapse = ", "))
    stop("Missing required ICIQ-UI SF columns: ",
         paste(missing_items, collapse = ", "))
  }

  # Validate missing threshold
  assertthat::assert_that(is.numeric(missing_threshold) &&
                            missing_threshold >= 0 &&
                            missing_threshold <= 1,
                          msg = "missing_threshold must be a value between 0 and 1")

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

#' Extract core ICIQ-UI SF items
#'
#' @param questionnaire_data Data frame containing questionnaire responses
#' @param core_items Character vector of core item column names
#' @return Data frame with core ICIQ-UI SF items
#' @noRd
extract_core_iciq_data <- function(questionnaire_data, core_items) {
  logger::log_debug("Extracting core ICIQ-UI SF items")

  # Extract core items (frequency, volume, impact)
  core_data <- questionnaire_data[, core_items, drop = FALSE]

  logger::log_debug("Extracted %d core items", length(core_items))
  return(core_data)
}

#' Process data types for ICIQ-UI SF items
#'
#' @param core_data Data frame containing core ICIQ-UI SF items
#' @param core_items Character vector of core item column names
#' @return Data frame with processed data types
#' @noRd
process_iciq_data_types <- function(core_data, core_items) {
  logger::log_debug("Processing data types for ICIQ-UI SF items")

  processed_data <- core_data

  # Define valid ranges for each item
  valid_ranges <- list(
    "frequency" = list(min = 0, max = 5),
    "volume" = list(min = 0, max = 6),
    "impact" = list(min = 0, max = 10)
  )

  # Map column names to item types
  column_types <- c(
    frequency = core_items[1],
    volume = core_items[2],
    impact = core_items[3]
  )

  # Process each column
  for (item_type in names(column_types)) {
    col <- column_types[[item_type]]
    min_val <- valid_ranges[[item_type]]$min
    max_val <- valid_ranges[[item_type]]$max

    # Convert to numeric if not already
    if (!is.numeric(processed_data[[col]])) {
      logger::log_warn("Converting column %s to numeric", col)
      processed_data[[col]] <- as.numeric(processed_data[[col]])
    }

    # Check for values outside valid range
    valid_range <- processed_data[[col]] >= min_val &
      processed_data[[col]] <= max_val |
      is.na(processed_data[[col]])

    if (!all(valid_range)) {
      invalid_indices <- which(!valid_range)
      invalid_values <- processed_data[[col]][invalid_indices]

      logger::log_warn("Column %s contains %d values outside valid range (%d-%d)",
                       col, length(invalid_indices), min_val, max_val)

      # Clip values to valid range
      processed_data[[col]][!is.na(processed_data[[col]]) &
                              processed_data[[col]] < min_val] <- min_val
      processed_data[[col]][!is.na(processed_data[[col]]) &
                              processed_data[[col]] > max_val] <- max_val

      logger::log_warn("Invalid values in column %s have been clipped to range %d-%d",
                       col, min_val, max_val)
    }
  }

  logger::log_debug("Data type processing completed")
  return(processed_data)
}

#' Calculate ICIQ-UI SF scores and handle missing data
#'
#' @param core_data Data frame with core ICIQ-UI SF items
#' @param core_items Character vector of core item column names
#' @param missing_threshold Maximum allowed missing proportion
#' @return Data frame with ICIQ-UI SF scores
#' @noRd
calculate_iciq_scores <- function(core_data, core_items, missing_threshold) {
  logger::log_debug("Calculating ICIQ-UI SF scores")

  # Rename columns for clarity
  colnames(core_data) <- c("frequency_score", "volume_score", "impact_score")

  # Count missing values
  missing_count <- rowSums(is.na(core_data))
  total_items <- ncol(core_data)
  missing_proportion <- missing_count / total_items

  # Calculate minimum required responses
  max_missing <- floor(total_items * missing_threshold)
  logger::log_debug("Maximum allowed missing items: %d out of %d",
                    max_missing, total_items)

  # Calculate valid rows (those with sufficient responses)
  valid_rows <- missing_count <= max_missing
  logger::log_debug("%d out of %d rows have valid ICIQ-UI SF data",
                    sum(valid_rows), length(valid_rows))

  # Calculate total ICIQ-UI SF score
  total_scores <- rep(NA_real_, nrow(core_data))

  if (any(valid_rows)) {
    # For valid rows, handle any missing values within threshold
    imputed_data <- core_data

    # Simple imputation for rows with some missing but below threshold
    has_some_missing <- missing_count > 0 & missing_count <= max_missing

    if (any(has_some_missing)) {
      logger::log_debug("Imputing missing values for %d rows with partial data",
                        sum(has_some_missing))

      # For each row with some missing but valid
      for (i in which(has_some_missing)) {
        # Get mean of non-missing items for the row (simple imputation)
        row_mean <- mean(unlist(core_data[i, ]), na.rm = TRUE)

        # Impute missing values with row mean
        for (col in colnames(core_data)) {
          if (is.na(imputed_data[i, col])) {
            imputed_data[i, col] <- row_mean
            logger::log_debug("Row %d: Imputed %s with value %.1f",
                              i, col, row_mean)
          }
        }
      }
    }

    # Calculate total score for valid rows
    total_scores[valid_rows] <- rowSums(imputed_data[valid_rows, , drop = FALSE])

    logger::log_debug("Total ICIQ-UI SF scores range from %.1f to %.1f",
                      min(total_scores, na.rm = TRUE),
                      max(total_scores, na.rm = TRUE))
  }

  # Create results data frame
  score_data <- cbind(
    core_data,
    data.frame(
      iciq_score = total_scores,
      iciq_missing = as.integer(missing_count),  # Explicitly convert to integer
      iciq_valid = valid_rows
    )
  )

  logger::log_debug("ICIQ-UI SF score calculation completed")
  return(score_data)
}

#' Extract and process situation data (item 4)
#'
#' @param questionnaire_data Data frame containing questionnaire responses
#' @param situation_prefix Prefix for situation columns
#' @return Data frame with processed situation data
#' @noRd
extract_situation_data <- function(questionnaire_data, situation_prefix) {
  logger::log_debug("Extracting and processing situation data")

  # Find situation columns
  situation_cols <- grep(paste0("^", situation_prefix),
                         names(questionnaire_data), value = TRUE)

  if (length(situation_cols) == 0) {
    logger::log_warn("No situation columns found with prefix: %s", situation_prefix)
    return(NULL)
  }

  logger::log_debug("Found %d situation columns", length(situation_cols))

  # Extract situation data
  situation_data <- questionnaire_data[, situation_cols, drop = FALSE]

  # Process each column to ensure it's logical/binary
  for (col in names(situation_data)) {
    # If not already logical, convert to logical
    if (!is.logical(situation_data[[col]])) {
      # Convert to numeric first (if character)
      if (!is.numeric(situation_data[[col]])) {
        situation_data[[col]] <- as.numeric(situation_data[[col]])
      }

      # Convert to logical (0 = FALSE, any non-zero = TRUE)
      situation_data[[col]] <- !is.na(situation_data[[col]]) &
        situation_data[[col]] != 0

      logger::log_debug("Converted column %s to logical", col)
    }

    # Replace NA with FALSE (not selected)
    situation_data[[col]][is.na(situation_data[[col]])] <- FALSE
  }

  # Define standard situation labels
  standard_situations <- c(
    "before_toilet" = "leaks before reaching toilet",
    "cough_sneeze" = "leaks when coughing/sneezing",
    "asleep" = "leaks when asleep",
    "physical" = "leaks with physical activity",
    "after_urinating" = "leaks when finished urinating",
    "no_reason" = "leaks for no obvious reason",
    "all_time" = "leaks all the time"
  )

  # Map situation columns to standard labels if possible
  if (length(situation_cols) <= length(standard_situations)) {
    # Create new column names
    new_names <- paste0("situation_",
                        names(standard_situations)[1:length(situation_cols)])
    colnames(situation_data) <- new_names

    # Create attribute with situation descriptions
    attr(situation_data, "situation_labels") <-
      standard_situations[1:length(situation_cols)]
  } else {
    # If more situations than standard, use generic names
    colnames(situation_data) <- paste0("situation_", 1:length(situation_cols))
  }

  logger::log_debug("Situation data processing completed")
  return(situation_data)
}

#' Combine score data with situation data
#'
#' @param score_data Data frame with ICIQ-UI SF scores
#' @param situation_data Data frame with situation data
#' @return Combined data frame
#' @noRd
combine_with_situations <- function(score_data, situation_data) {
  if (is.null(situation_data)) {
    logger::log_debug("No situation data to combine")
    return(score_data)
  }

  logger::log_debug("Combining score data with situation data")
  combined_data <- cbind(score_data, situation_data)

  return(combined_data)
}

#' Combine all results into final output
#'
#' @param questionnaire_data Original questionnaire data
#' @param id_data Data frame with ID column or NULL
#' @param score_data Data frame with calculated scores
#' @return Combined data frame with all data
#' @noRd
combine_iciq_results <- function(questionnaire_data, id_data, score_data) {
  logger::log_debug("Combining results into final output")

  if (is.null(id_data)) {
    # If no ID column was specified, just add score data to original data
    final_result <- questionnaire_data
    # Add new columns - avoid duplicating any columns
    new_cols <- setdiff(names(score_data), names(questionnaire_data))
    if (length(new_cols) > 0) {
      final_result <- cbind(final_result, score_data[, new_cols, drop = FALSE])
    }
  } else {
    # If ID column was specified, construct result with ID and scores
    final_result <- cbind(id_data, score_data)
  }

  return(final_result)
}
