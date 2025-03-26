#' Score the Patient Global Impression of Improvement (PGI-I)
#'
#' @description
#' Scores the PGI-I, a single-item measure assessing patient-reported
#' improvement following treatment. The function includes robust input validation,
#' detailed logging, and handles missing values according to scoring guidelines.
#'
#' @param patient_data A data frame containing the PGI-I response variable.
#' @param item_name The column name in the data frame that contains the PGI-I response.
#'   This must be a valid column in patient_data containing values 1-7 or NA.
#' @param id_column Character string specifying the column name containing patient
#'   identifiers. Default is "Record_ID".
#' @param keep_n_valid Logical, whether to return the number of valid, non-missing
#'   responses in the output. Default is FALSE.
#' @param verbose Logical indicating whether to print detailed logging messages.
#'   Default is FALSE.
#' @param output_file Character string specifying the path to save the results.
#'   If NULL (default), results are not saved to a file.
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
#' @return A data frame containing:
#' \itemize{
#'   \item \strong{pgii_score} - The original PGI-I response score (1-7 or NA)
#'   \item \strong{pgii_improved} - A binary variable indicating if the patient
#'     reported improvement (scores between 1 and 3 are considered improvement,
#'     scores between 4 and 7 are considered no improvement/worsening)
#'   \item \strong{pgii_n_valid} - (Optional) The number of valid responses
#' }
#'
#' @examples
#' # Example 1: Basic usage with default parameters
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
#' # Example 2: With custom ID column and including count of valid responses
#' patient_data <- data.frame(
#'   PatientID = c("A", "B", "C", "D", "E"),
#'   Treatment = c("Drug", "Placebo", "Drug", "Drug", "Placebo"),
#'   GlobalImpression = c(2, 4, 1, 3, 6)
#' )
#'
#' pgii_scores <- score_pgii(
#'   patient_data = patient_data,
#'   item_name = "GlobalImpression",
#'   id_column = "PatientID",
#'   keep_n_valid = TRUE,
#'   verbose = TRUE
#' )
#' print(pgii_scores)
#'
#' # Example 3: Save results to file
#' patient_data <- data.frame(
#'   Record_ID = 1:5,
#'   pgii_response = c(2, 3, 4, 1, NA)
#' )
#'
#' temp_file <- tempfile(fileext = ".csv")
#' pgii_scores <- score_pgii(
#'   patient_data = patient_data,
#'   item_name = "pgii_response",
#'   output_file = temp_file,
#'   verbose = TRUE
#' )
#' # Results saved to temp_file
#'
#' @references
#' Yalcin, I., & Bump, R. C. (2003). Validation of two global impression questionnaires
#' for incontinence. \emph{American Journal of Obstetrics and Gynecology, 189}(1), 98-101.
#'
#' @importFrom dplyr mutate select
#' @importFrom tibble as_tibble
#' @importFrom utils write.csv
#' @importFrom logger log_info log_error log_debug log_warn log_threshold
#' @importFrom assertthat assert_that
#'
#' @export
score_pgii <- function(patient_data,
                       item_name,
                       id_column = "Record_ID",
                       keep_n_valid = FALSE,
                       verbose = FALSE,
                       output_file = NULL) {

  # Initialize logger
  if (verbose) {
    logger::log_threshold(logger::INFO)
  } else {
    logger::log_threshold(logger::ERROR)
  }

  # Log function inputs
  logger::log_info("Starting PGI-I score calculation")
  logger::log_info("Input parameters: item_name = %s, id_column = %s, keep_n_valid = %s, verbose = %s, output_file = %s",
                   item_name, id_column, keep_n_valid, verbose, ifelse(is.null(output_file), "NULL", output_file))

  # Validate inputs
  validate_pgii_inputs(patient_data, item_name, id_column)

  # Extract patient IDs and PGI-I responses
  pgii_data <- extract_pgii_data(patient_data, id_column, item_name)

  # Calculate PGI-I scores and improvement indicators
  pgii_results <- calculate_pgii_scores(pgii_data, id_column, item_name, keep_n_valid)

  # Log results summary
  logger::log_info("PGI-I scores calculated: %d valid, %d missing",
                   sum(!is.na(pgii_results$pgii_score)),
                   sum(is.na(pgii_results$pgii_score)))

  # Save to file if specified
  if (!is.null(output_file)) {
    logger::log_info("Saving results to file: %s", output_file)
    utils::write.csv(pgii_results, output_file, row.names = FALSE)
  }

  # Return results as tibble
  return(tibble::as_tibble(pgii_results))
}
