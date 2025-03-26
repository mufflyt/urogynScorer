# Fixed version of declaring global variables
# This should be placed at the top of your R/score_pfdi.R file,
# BEFORE the roxygen documentation of the main function

# Define global variables used in NSE contexts
utils::globalVariables(c("response", "non_missing", "mean_response"))
#' Calculate PFDI Scores from Minimal Dataset
#'
#' This function calculates Pelvic Floor Disability Index (PFDI) scores using
#' a minimal dataset of responses. It processes the data and returns calculated
#' subscale scores and the total PFDI score.
#'
#' @param input_data A data frame containing PFDI questionnaire responses.
#'        Column names should match the standard PFDI question format.
#' @param scale_factor Numeric. The scaling factor to apply to the scores.
#'        Default is 25.
#' @param include_subscales Logical. Whether to include subscale scores in the output.
#'        Default is TRUE.
#' @param verbose Logical. Whether to print detailed logging information.
#'        Default is FALSE.
#'
#' @return A data frame containing calculated PFDI scores with total score and
#'         optionally subscale scores (POPDI, CRADI, UDI).
#'
#' @importFrom dplyr mutate select summarize group_by across everything
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom assertthat assert_that
#' @importFrom logger log_info log_debug log_error
#'
#' @examples
#' # Example 1: Basic usage with default parameters
#' patient_data <- data.frame(
#'   id = 1,
#'   popdi_q1 = 2, popdi_q2 = 1, popdi_q3 = 0,
#'   cradi_q1 = 1, cradi_q2 = 0, cradi_q3 = 2,
#'   udi_q1 = 3, udi_q2 = 1, udi_q3 = 0
#' )
#' pfdi_scores <- calculate_pfdi_scores(
#'   input_data = patient_data,
#'   scale_factor = 25,
#'   include_subscales = TRUE,
#'   verbose = FALSE
#' )
#' print(pfdi_scores)
#' # Output:
#' #   id popdi_score cradi_score udi_score pfdi_total_score
#' #   1         25         25       33.3           83.3
#'
#' # Example 2: With verbose logging
#' pfdi_scores_logged <- calculate_pfdi_scores(
#'   input_data = patient_data,
#'   scale_factor = 25,
#'   include_subscales = TRUE,
#'   verbose = TRUE
#' )
#' # Logs will show detailed processing information
#'
#' # Example 3: Without subscales
#' pfdi_total_only <- calculate_pfdi_scores(
#'   input_data = patient_data,
#'   scale_factor = 25,
#'   include_subscales = FALSE,
#'   verbose = FALSE
#' )
#' print(pfdi_total_only)
#' # Output:
#' #   id pfdi_total_score
#' #   1           83.3
#'
#' @export
calculate_pfdi_scores <- function(input_data,
                                  scale_factor = 25,
                                  include_subscales = TRUE,
                                  verbose = FALSE) {
  # Setup logging based on verbose parameter
  if (verbose) {
    logger::log_threshold(logger::DEBUG)
    logger::log_info("Starting PFDI score calculation")
    logger::log_debug("Input parameters: scale_factor = {scale_factor}, include_subscales = {include_subscales}")
  }

  # Input validation
  assertthat::assert_that(is.data.frame(input_data),
                          msg = "input_data must be a data frame")
  assertthat::assert_that(is.numeric(scale_factor),
                          msg = "scale_factor must be numeric")
  assertthat::assert_that(is.logical(include_subscales),
                          msg = "include_subscales must be logical")

  # Log input data dimensions
  if (verbose) {
    logger::log_debug("Input data dimensions: {nrow(input_data)} rows, {ncol(input_data)} columns")
    logger::log_debug("Input data column names: {paste(colnames(input_data), collapse = ', ')}")
  }

  # Process the data using helper functions
  processed_scores <- tryCatch({
    # Extract domain questions
    if (verbose) logger::log_debug("Extracting question domains")

    domain_questions <- extract_domain_questions(input_data, verbose)

    # Calculate subscale scores
    if (verbose) logger::log_debug("Calculating subscale scores")

    subscale_scores <- calculate_subscale_scores(
      input_data = input_data,
      domain_questions = domain_questions,
      scale_factor = scale_factor,
      verbose = verbose
    )

    # Calculate total PFDI score
    if (verbose) logger::log_debug("Calculating total PFDI score")

    final_scores <- calculate_total_pfdi_score(
      subscale_scores = subscale_scores,
      include_subscales = include_subscales,
      verbose = verbose
    )

    if (verbose) {
      logger::log_info("PFDI score calculation completed successfully")
      logger::log_debug("Output data dimensions: {nrow(final_scores)} rows, {ncol(final_scores)} columns")
    }

    return(final_scores)
  },
  error = function(err) {
    logger::log_error("Error in PFDI score calculation: {err$message}")
    stop(paste("Failed to calculate PFDI scores:", err$message))
  })

  return(processed_scores)
}

#' @noRd
extract_domain_questions <- function(input_data, verbose = FALSE) {
  if (verbose) logger::log_debug("Identifying PFDI question columns")

  # Identify columns for each domain
  popdi_cols <- grep("^popdi_q\\d+", names(input_data), value = TRUE)
  cradi_cols <- grep("^cradi_q\\d+", names(input_data), value = TRUE)
  udi_cols <- grep("^udi_q\\d+", names(input_data), value = TRUE)

  if (verbose) {
    logger::log_debug("Found {length(popdi_cols)} POPDI questions")
    logger::log_debug("Found {length(cradi_cols)} CRADI questions")
    logger::log_debug("Found {length(udi_cols)} UDI questions")
  }

  # Check if any domain questions were found
  if (length(c(popdi_cols, cradi_cols, udi_cols)) == 0) {
    logger::log_error("No PFDI question columns found in the input data")
    stop("No PFDI question columns found in the input data. Column names should include 'popdi_q', 'cradi_q', or 'udi_q' prefixes.")
  }

  # Return the domain questions
  return(list(
    popdi = popdi_cols,
    cradi = cradi_cols,
    udi = udi_cols
  ))
}

#' @noRd
calculate_subscale_scores <- function(input_data, domain_questions, scale_factor, verbose = FALSE) {
  # Extract ID column - typically first column or named 'id'
  id_col <- names(input_data)[1]  # Default to first column
  if ("id" %in% names(input_data)) {
    id_col <- "id"
  }

  if (verbose) logger::log_debug("Using '{id_col}' as the identifier column")

  # Initialize results data frame with ID
  subscale_scores <- dplyr::select(input_data, dplyr::all_of(id_col))

  # Calculate POPDI score
  if (length(domain_questions$popdi) > 0) {
    if (verbose) logger::log_debug("Calculating POPDI score from {length(domain_questions$popdi)} questions")

    popdi_mean <- dplyr::rowMeans(input_data[, domain_questions$popdi, drop = FALSE], na.rm = TRUE)
    subscale_scores <- dplyr::mutate(subscale_scores,
                                     popdi_score = round(popdi_mean * scale_factor, 1))

    if (verbose) logger::log_debug("POPDI score calculation complete")
  }

  # Calculate CRADI score
  if (length(domain_questions$cradi) > 0) {
    if (verbose) logger::log_debug("Calculating CRADI score from {length(domain_questions$cradi)} questions")

    cradi_mean <- dplyr::rowMeans(input_data[, domain_questions$cradi, drop = FALSE], na.rm = TRUE)
    subscale_scores <- dplyr::mutate(subscale_scores,
                                     cradi_score = round(cradi_mean * scale_factor, 1))

    if (verbose) logger::log_debug("CRADI score calculation complete")
  }

  # Calculate UDI score
  if (length(domain_questions$udi) > 0) {
    if (verbose) logger::log_debug("Calculating UDI score from {length(domain_questions$udi)} questions")

    udi_mean <- dplyr::rowMeans(input_data[, domain_questions$udi, drop = FALSE], na.rm = TRUE)
    subscale_scores <- dplyr::mutate(subscale_scores,
                                     udi_score = round(udi_mean * scale_factor, 1))

    if (verbose) logger::log_debug("UDI score calculation complete")
  }

  return(subscale_scores)
}

#' @noRd
calculate_total_pfdi_score <- function(subscale_scores, include_subscales, verbose = FALSE) {
  if (verbose) logger::log_debug("Calculating total PFDI score")

  # Identify score columns
  score_cols <- grep("_score$", names(subscale_scores), value = TRUE)

  # Calculate total PFDI score
  total_scores <- dplyr::mutate(
    subscale_scores,
    pfdi_total_score = round(rowSums(
      dplyr::select(subscale_scores, dplyr::all_of(score_cols)),
      na.rm = TRUE
    ), 1)
  )

  if (verbose) {
    logger::log_debug("Total PFDI score calculation complete")
    logger::log_debug("Score columns used: {paste(score_cols, collapse = ', ')}")
  }

  # Return only total score if include_subscales is FALSE
  if (!include_subscales) {
    # Identify ID column
    id_col <- names(subscale_scores)[1]
    if ("id" %in% names(subscale_scores)) {
      id_col <- "id"
    }

    if (verbose) logger::log_debug("Removing subscale scores from output")

    # Keep only ID and total score
    total_scores <- dplyr::select(
      total_scores,
      dplyr::all_of(id_col),
      pfdi_total_score
    )
  }

  return(total_scores)
}
