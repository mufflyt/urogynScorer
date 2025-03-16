#' Score the Patient Global Impression of Improvement (PGI-I)
#'
#' @description Scores the PGI-I, a single-item measure assessing patient-reported
#' improvement following treatment.
#'
#' @param df A data frame containing the PGI-I response variable.
#' @param item_name The column name in the data frame that contains the PGI-I response.
#' @param keepNvalid Logical, whether to return the number of valid, non-missing
#' responses in the output. Default is `FALSE`.
#'
#' @details
#' The PGI-I is a single-item global rating scale where patients rate their
#' improvement after treatment on a scale from 1 (very much better) to 7 (very much worse).
#' Lower scores indicate better perceived improvement.
#'
#' @section How Missing Data is Handled:
#' If the PGI-I response is missing (`NA`), the function will return `NA` for the score.
#'
#' @return A data frame containing:
#' \itemize{
#'   \item \strong{pgii_score} - The original PGI-I response score (1-7).
#'   \item \strong{pgii_improved} - A binary variable indicating if the patient
#'     reported improvement (1 = improvement, 0 = no improvement/worsening).
#'   \item \strong{pgii_Nvalid} - (Optional) The number of valid responses.
#' }
#'
#' @references
#' Yalcin, I., & Bump, R. C. (2003). Validation of two global impression questionnaires
#' for incontinence. \emph{American Journal of Obstetrics and Gynecology, 189}(1), 98-101.
#'
#' @export
#'
#' @examples
#' df <- data.frame(pgii_response = c(1, 2, 3, 4, 5, 6, 7, NA))
#' score_pgii(df, "pgii_response")

score_pgii <- function(df, item_name, keepNvalid = FALSE) {
  assertthat::assert_that(is.data.frame(df), msg = "Input must be a data frame.")
  assertthat::assert_that(item_name %in% names(df), msg = "Specified column not found in data frame.")
  assertthat::assert_that(all(df[[item_name]] %in% c(1:7, NA)),
                          msg = "PGI-I responses must be integers between 1 and 7 or NA.")

  # Extract the PGI-I response
  pgii_score <- df[[item_name]]

  # Create a binary improvement indicator (1 = improved, 0 = no improvement/worsened)
  pgii_improved <- ifelse(pgii_score <= 3, 1, 0)

  # Count valid responses
  pgii_Nvalid <- sum(!is.na(pgii_score))

  # Create output data frame
  result <- data.frame(pgii_score, pgii_improved)

  # Include count of valid responses if requested
  if (keepNvalid) {
    result$pgii_Nvalid <- pgii_Nvalid
  }

  return(result)
}
