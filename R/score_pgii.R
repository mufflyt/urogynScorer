#' Score the Patient Global Impression of Improvement (PGI-I)
#' @description
#' Scores the PGI-I, a single-item measure assessing patient-reported
#' improvement following treatment. The function includes robust input validation,
#' detailed logging, and handles missing values according to scoring guidelines.
#'
#' @param patient_data A data frame containing the PGI-I response variable.
#' @param item_name The column name in the data frame that contains the PGI-I response.
#'   This must be a valid column in `patient_data` containing values 1-7 or NA.
#' @param id_column Character string specifying the column name containing patient
#'   identifiers. Default is `"Record_ID"`.
#' @param keep_n_valid Logical, whether to return the number of valid, non-missing
#'   responses in the output. Default is `FALSE`.
#' @param keepNvalid Deprecated. Use `keep_n_valid` instead.
#' @param verbose Logical indicating whether to print detailed logging messages.
#'   Default is `FALSE`.
#' @param output_file Character string specifying the path to save the results.
#'   If `NULL` (default), results are not saved to a file.
#'
#' @details
#' The PGI-I is a single-item global rating scale where patients rate their
#' improvement after treatment on a scale from 1 (very much better) to 7 (very much worse).
#' Lower scores indicate better perceived improvement.
#'
#' PGI-I response scale:
#' \itemize{
#'   \item 1: Very much better
#'   \item 2: Much better
#'   \item 3: A little better
#'   \item 4: No change
#'   \item 5: A little worse
#'   \item 6: Much worse
#'   \item 7: Very much worse
#' }
#'
#' @section How Missing Data is Handled:
#' If the PGI-I response is missing (NA), the function will return NA for the score
#' and the binary improvement indicator.
#'
#' @return A tibble containing:
#' \itemize{
#'   \item The patient identifier column (as specified by `id_column`)
#'   \item \strong{pgii_score} - The original PGI-I response score (1-7 or NA)
#'   \item \strong{pgii_improved} - A binary variable indicating if the patient
#'     reported improvement (1 = improvement [scores 1-3], 0 = no improvement/worsening [scores 4-7])
#'   \item \strong{pgii_n_valid} - (Optional) The number of valid responses
#' }
#'
#' @examples
#' # Example usage:
#' patient_data <- data.frame(
#'   Record_ID = c("P001", "P002", "P003", "P004"),
#'   pgii_response = c(1, 3, 5, NA)
#' )
#'
#' pgii_scores <- score_pgii(
#'   patient_data = patient_data,
#'   item_name = "pgii_response",
#'   id_column = "Record_ID",
#'   keep_n_valid = FALSE,
#'   verbose = TRUE
#' )
#' print(pgii_scores)
#'
#' @references
#' Yalcin, I., & Bump, R. C. (2003). Validation of two global impression questionnaires
#' for incontinence. \emph{American Journal of Obstetrics and Gynecology, 189}(1), 98-101.
#'
#' @importFrom dplyr mutate select
#' @importFrom tibble as_tibble
#' @importFrom utils write.csv
#' @importFrom logger log_info log_warn log_threshold
#' @importFrom assertthat assert_that
#'
#' @export
score_pgii <- function(patient_data,
                       item_name,
                       id_column = "Record_ID",
                       keep_n_valid = FALSE,
                       keepNvalid = NULL,
                       verbose = FALSE,
                       output_file = NULL) {

  # Handle backward compatibility for deprecated parameter keepNvalid
  if (!is.null(keepNvalid)) {
    keep_n_valid <- keepNvalid
    warning("Parameter 'keepNvalid' is deprecated. Please use 'keep_n_valid' instead.", call. = FALSE)
  }

  # Initialize logger verbosity
  if (verbose) {
    logger::log_threshold(logger::INFO)
  } else {
    logger::log_threshold(logger::ERROR)
  }

  # Log function start
  logger::log_info("Starting PGI-I score calculation")
  logger::log_info("Input parameters: item_name = '%s', id_column = '%s', keep_n_valid = %s, verbose = %s, output_file = %s",
                   item_name, id_column, keep_n_valid, verbose, ifelse(is.null(output_file), "NULL", output_file))

  # Validate input data
  assertthat::assert_that(is.data.frame(patient_data), msg = "Input must be a data frame.")
  assertthat::assert_that(item_name %in% colnames(patient_data),
                          msg = paste0("Column '", item_name, "' not found in data."))
  assertthat::assert_that(id_column %in% colnames(patient_data),
                          msg = paste0("Column '", id_column, "' not found in data."))

  # Extract PGI-I responses
  pgii_data <- dplyr::select(patient_data, dplyr::all_of(c(id_column, item_name)))

  # Validate PGI-I response values
  valid_values <- c(1, 2, 3, 4, 5, 6, 7, NA)
  assertthat::assert_that(
    all(pgii_data[[item_name]] %in% valid_values, na.rm = TRUE),
    msg = paste0("PGI-I responses must be integers between 1 and 7 or NA. Found invalid values in column '", item_name, "'.")
  )

  # Compute improvement indicator (1 = improved [1-3], 0 = not improved/worsened [4-7])
  pgii_score <- pgii_data[[item_name]]
  pgii_improved <- ifelse(is.na(pgii_score), NA, ifelse(pgii_score <= 3, 1, 0))

  # Count valid responses
  pgii_n_valid <- sum(!is.na(pgii_score))

  # Create output tibble
  pgii_results <- tibble::tibble(
    !!id_column := pgii_data[[id_column]],
    pgii_score = pgii_score,
    pgii_improved = pgii_improved
  )

  # Add count of valid responses if requested
  if (keep_n_valid) {
    pgii_results$pgii_n_valid <- pgii_n_valid
  }

  # Log summary
  logger::log_info("PGI-I scores calculated: %d valid, %d missing",
                   as.integer(sum(!is.na(pgii_results$pgii_score))),
                   as.integer(sum(is.na(pgii_results$pgii_score))))

  # Save to file if specified
  if (!is.null(output_file)) {
    logger::log_info("Saving results to file: %s", output_file)
    utils::write.csv(pgii_results, output_file, row.names = FALSE)
  }

  return(pgii_results)
}
