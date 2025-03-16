# Fixed version of declaring global variables
# This should be placed at the top of your R/score_pfdi.R file,
# BEFORE the roxygen documentation of the main function

# Define global variables used in NSE contexts
utils::globalVariables(c("response", "non_missing", "mean_response"))
#' Calculate PFDI-20 scores and subscales
#'
#' @description
#' This function calculates the Pelvic Floor Distress Inventory (PFDI-20) total
#' score and subscale scores (POPDI-6, CRADI-8, and UDI-6) from raw questionnaire
#' data. The function includes input validation, handles missing values according
#' to scoring guidelines, and provides detailed logging.
#'
#' @param patient_data A data frame containing PFDI-20 questionnaire responses.
#'   Must include columns PFDI-1 through PFDI-20 or the explicitly named subscale
#'   items (PFDI-POPDI-1 through PFDI-POPDI-6, PFDI-CRADI-1 through PFDI-CRADI-8,
#'   and PFDI-UDI-1 through PFDI-UDI-6).
#' @param id_column Character string specifying the column name containing patient
#'   identifiers. Default is "Record_ID".
#' @param missing_threshold Numeric value between 0 and 1 specifying the maximum
#'   proportion of missing values allowed per subscale before the score is considered
#'   invalid. Default is 0.5 (50%).
#' @param verbose Logical indicating whether to print detailed logging messages.
#'   Default is FALSE.
#' @param output_file Character string specifying the path to save the results.
#'   If NULL (default), results are not saved to a file.
#'
#' @return A tibble containing patient IDs and calculated scores for PFDI-20 total
#'   and the three subscales (POPDI-6, CRADI-8, and UDI-6).
#'
#' @examples
#' # Example 1: Create sample data with all required PFDI columns
#' pfdi_cols <- c(
#'   "PFDI-1", "PFDI-2", "PFDI-3", "PFDI-4", "PFDI-5", "PFDI-6",
#'   "PFDI-7", "PFDI-8", "PFDI-9", "PFDI-10", "PFDI-11", "PFDI-12",
#'   "PFDI-13", "PFDI-14", "PFDI-15", "PFDI-16", "PFDI-17", "PFDI-18",
#'   "PFDI-19", "PFDI-20"
#' )
#'
#' # Create data with exactly 3 patients, all 20 PFDI questions for each
#' sample_data <- data.frame(Record_ID = 1:3)
#' for (col in pfdi_cols) {
#'   sample_data[[col]] <- sample(0:4, 3, replace = TRUE)
#' }
#'
#' # Example 1: Basic usage with default parameters
#' pfdi_scores <- calculate_pfdi_scores(
#'   patient_data = sample_data,
#'   id_column = "Record_ID",
#'   missing_threshold = 0.5,
#'   verbose = TRUE
#' )
#' print(pfdi_scores)
#'
#' # Example 2: Custom ID column and stricter missing value threshold
#' # Add a new ID column
#' sample_data$MRN <- sample_data$Record_ID + 1000
#'
#' # Calculate with custom ID column
#' pfdi_scores <- calculate_pfdi_scores(
#'   patient_data = sample_data,
#'   id_column = "MRN",
#'   missing_threshold = 0.25,
#'   verbose = TRUE
#' )
#' print(pfdi_scores)
#'
#' # Example 3: Handle missing values
#' # Create data with some missing values
#' missing_data <- sample_data
#' missing_data[1, "PFDI-1"] <- NA
#' missing_data[2, "PFDI-5"] <- NA
#' missing_data[2, "PFDI-10"] <- NA
#'
#' # Calculate with default missing threshold
#' pfdi_scores <- calculate_pfdi_scores(
#'   patient_data = missing_data,
#'   id_column = "Record_ID",
#'   missing_threshold = 0.5,
#'   verbose = TRUE
#' )
#' print(pfdi_scores)
#'
#' @importFrom dplyr mutate select filter group_by summarize across all_of left_join
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble tibble as_tibble
#' @importFrom utils write.csv
#' @importFrom logger log_info log_error log_debug log_warn log_threshold
#' @importFrom magrittr %>%
#' @importFrom rlang .data :=
#' @importFrom assertthat assert_that
#'
#' @export
calculate_pfdi_scores <- function(patient_data,
                                  id_column = "Record_ID",
                                  missing_threshold = 0.5,
                                  verbose = FALSE,
                                  output_file = NULL) {

  # Initialize logger
  if (verbose) {
    logger::log_threshold(logger::INFO)
  } else {
    logger::log_threshold(logger::ERROR)
  }

  # Log function inputs
  logger::log_info("Starting PFDI score calculation")
  logger::log_info("Input parameters: id_column = %s, missing_threshold = %f, verbose = %s, output_file = %s",
                   id_column, missing_threshold, verbose, ifelse(is.null(output_file), "NULL", output_file))

  # Validate inputs
  validate_pfdi_inputs(patient_data, id_column, missing_threshold)

  # Define PFDI question mappings to subscales
  logger::log_debug("Setting up PFDI question to subscale mappings")
  popdi_questions <- c("PFDI-1", "PFDI-2", "PFDI-3", "PFDI-4", "PFDI-5", "PFDI-6")
  cradi_questions <- c("PFDI-7", "PFDI-8", "PFDI-9", "PFDI-10", "PFDI-11", "PFDI-12", "PFDI-13", "PFDI-14")
  udi_questions <- c("PFDI-15", "PFDI-16", "PFDI-17", "PFDI-18", "PFDI-19", "PFDI-20")

  # Check if alternative column names are used
  alt_popdi_questions <- c("PFDI-POPDI-1", "PFDI-POPDI-2", "PFDI-POPDI-3", "PFDI-POPDI-4", "PFDI-POPDI-5", "PFDI-POPDI-6")
  alt_cradi_questions <- c("PFDI-CRADI-1", "PFDI-CRADI-2", "PFDI-CRADI-3", "PFDI-CRADI-4", "PFDI-CRADI-5", "PFDI-CRADI-6", "PFDI-CRADI-7", "PFDI-CRADI-8")
  alt_udi_questions <- c("PFDI-UDI-1", "PFDI-UDI-2", "PFDI-UDI-3", "PFDI-UDI-4", "PFDI-UDI-5", "PFDI-UDI-6")

  # Determine which set of column names to use
  column_names <- colnames(patient_data)

  if (all(popdi_questions %in% column_names)) {
    logger::log_info("Using standard PFDI column names (PFDI-1 to PFDI-20)")
    use_popdi_questions <- popdi_questions
    use_cradi_questions <- cradi_questions
    use_udi_questions <- udi_questions
  } else if (all(alt_popdi_questions %in% column_names)) {
    logger::log_info("Using alternative PFDI column names (PFDI-POPDI-x, PFDI-CRADI-x, PFDI-UDI-x)")
    use_popdi_questions <- alt_popdi_questions
    use_cradi_questions <- alt_cradi_questions
    use_udi_questions <- alt_udi_questions
  } else {
    logger::log_error("Cannot find all required PFDI columns in the dataset")
    stop("Cannot find all required PFDI columns in the dataset")
  }

  # Calculate subscale scores
  logger::log_info("Calculating POPDI-6 subscale scores")
  popdi_scores <- calculate_subscale_score(
    patient_data = patient_data,
    id_column = id_column,
    questions = use_popdi_questions,
    subscale_name = "POPDI_6",
    max_score = 25, # Now correctly using 25 instead of 100
    n_questions = 6,
    missing_threshold = missing_threshold
  )

  logger::log_info("Calculating CRADI-8 subscale scores")
  cradi_scores <- calculate_subscale_score(
    patient_data = patient_data,
    id_column = id_column,
    questions = use_cradi_questions,
    subscale_name = "CRADI_8",
    max_score = 25, # Now correctly using 25 instead of 100
    n_questions = 8,
    missing_threshold = missing_threshold
  )

  logger::log_info("Calculating UDI-6 subscale scores")
  udi_scores <- calculate_subscale_score(
    patient_data = patient_data,
    id_column = id_column,
    questions = use_udi_questions,
    subscale_name = "UDI_6",
    max_score = 25, # Now correctly using 25 instead of 100
    n_questions = 6,
    missing_threshold = missing_threshold
  )

  # Combine all scores
  logger::log_info("Combining all subscale scores")
  pfdi_scores <- dplyr::left_join(
    popdi_scores,
    cradi_scores,
    by = id_column
  ) %>%
    dplyr::left_join(
      udi_scores,
      by = id_column
    ) %>%
    dplyr::mutate(
      PFDI_20_total = rowSums(dplyr::across(c("POPDI_6", "CRADI_8", "UDI_6")), na.rm = FALSE)
    )

  # Convert to tibble and log the output structure
  pfdi_scores <- tibble::as_tibble(pfdi_scores)
  logger::log_info("Final PFDI scores calculated for %d patients", nrow(pfdi_scores))
  logger::log_debug("Output structure: %s",
                    paste(colnames(pfdi_scores), collapse = ", "))

  # Save to file if specified
  if (!is.null(output_file)) {
    logger::log_info("Saving results to file: %s", output_file)
    # Use utils::write.csv instead of readr::write_csv for fewer dependencies
    utils::write.csv(pfdi_scores, output_file, row.names = FALSE)
  }

  return(pfdi_scores)
}

#' Calculate a subscale score for PFDI questionnaire
#'
#' @param patient_data Data frame containing patient responses
#' @param id_column Column name for patient identifiers
#' @param questions Vector of column names for subscale questions
#' @param subscale_name Name of the subscale for output
#' @param max_score Maximum possible subscale score
#' @param n_questions Number of questions in the subscale
#' @param missing_threshold Maximum proportion of missing values allowed
#'
#' @return A data frame with patient IDs and subscale scores
#'
#' @noRd
calculate_subscale_score <- function(patient_data,
                                     id_column,
                                     questions,
                                     subscale_name,
                                     max_score,
                                     n_questions,
                                     missing_threshold) {

  # Log the subscale calculation
  logger::log_debug("Calculating %s score with %d questions and max score %d",
                    subscale_name, n_questions, max_score)

  # Subset the data to include only the ID column and subscale questions
  subscale_data <- patient_data %>%
    dplyr::select(dplyr::all_of(c(id_column, questions)))

  # Calculate the mean response value for each patient, handling missing values
  subscale_scores <- subscale_data %>%
    tidyr::pivot_longer(
      cols = -dplyr::all_of(id_column),
      names_to = "question",
      values_to = "response"
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(id_column))) %>%
    dplyr::summarize(
      non_missing = sum(!is.na(response)),
      mean_response = ifelse(
        non_missing >= (1 - missing_threshold) * n_questions,
        mean(response, na.rm = TRUE),
        NA
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      !!rlang::sym(subscale_name) := mean_response * max_score
    ) %>%
    dplyr::select(-non_missing, -mean_response)

  # Log the results summary
  logger::log_info("%s scores calculated: %d valid, %d missing",
                   subscale_name,
                   sum(!is.na(subscale_scores[[subscale_name]])),
                   sum(is.na(subscale_scores[[subscale_name]])))

  return(subscale_scores)
}

#' Validate inputs for PFDI score calculation
#'
#' @param patient_data Data frame containing patient responses
#' @param id_column Column name for patient identifiers
#' @param missing_threshold Maximum proportion of missing values allowed
#'
#' @noRd
validate_pfdi_inputs <- function(patient_data, id_column, missing_threshold) {
  logger::log_debug("Validating input parameters")

  # Check if patient_data is a data frame
  assertthat::assert_that(
    is.data.frame(patient_data),
    msg = "patient_data must be a data frame"
  )

  # Check if id_column exists in patient_data
  assertthat::assert_that(
    id_column %in% colnames(patient_data),
    msg = paste0("id_column '", id_column, "' not found in patient_data")
  )

  # Check if missing_threshold is valid
  assertthat::assert_that(
    is.numeric(missing_threshold),
    msg = "missing_threshold must be numeric"
  )

  assertthat::assert_that(
    missing_threshold >= 0 && missing_threshold <= 1,
    msg = "missing_threshold must be between 0 and 1"
  )

  # Check if patient_data has required PFDI columns
  std_columns <- c("PFDI-1", "PFDI-2", "PFDI-3", "PFDI-4", "PFDI-5",
                   "PFDI-6", "PFDI-7", "PFDI-8", "PFDI-9", "PFDI-10",
                   "PFDI-11", "PFDI-12", "PFDI-13", "PFDI-14", "PFDI-15",
                   "PFDI-16", "PFDI-17", "PFDI-18", "PFDI-19", "PFDI-20")

  alt_columns <- c(
    "PFDI-POPDI-1", "PFDI-POPDI-2", "PFDI-POPDI-3", "PFDI-POPDI-4", "PFDI-POPDI-5", "PFDI-POPDI-6",
    "PFDI-CRADI-1", "PFDI-CRADI-2", "PFDI-CRADI-3", "PFDI-CRADI-4", "PFDI-CRADI-5", "PFDI-CRADI-6", "PFDI-CRADI-7", "PFDI-CRADI-8",
    "PFDI-UDI-1", "PFDI-UDI-2", "PFDI-UDI-3", "PFDI-UDI-4", "PFDI-UDI-5", "PFDI-UDI-6"
  )

  has_std_columns <- all(std_columns %in% colnames(patient_data))
  has_alt_columns <- all(alt_columns %in% colnames(patient_data))

  assertthat::assert_that(
    has_std_columns || has_alt_columns,
    msg = paste("patient_data must contain either all standard PFDI columns (PFDI-1 through PFDI-20)",
                "or all alternative columns (PFDI-POPDI-x, PFDI-CRADI-x, PFDI-UDI-x)")
  )

  logger::log_info("Input validation successful")
}
