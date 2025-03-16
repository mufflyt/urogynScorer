#' Sandvik Severity Index Calculation
#'
#' @description Computes the Sandvik Severity Index for urinary incontinence severity
#' based on frequency and amount of leakage.
#'
#' @param frequency Numeric vector with values between 0 and 4 representing the frequency
#' of incontinence episodes.
#'   - 0: Never
#'   - 1: Less than once a month
#'   - 2: A few times a month
#'   - 3: A few times a week
#'   - 4: Every day and/or night
#'
#' @param amount Numeric vector with values between 1 and 3 representing the amount of leakage.
#'   - 1: Drops
#'   - 2: Small amounts
#'   - 3: Large amounts
#'
#' @return A tibble with the following columns:
#' \itemize{
#'   \item \strong{Frequency} - The input frequency of incontinence.
#'   \item \strong{Amount} - The input amount of leakage.
#'   \item \strong{Severity_Index} - The computed Sandvik Severity Index (Frequency * Amount).
#'   \item \strong{Severity_Level} - The categorized severity level:
#'     \itemize{
#'       \item "Slight" (≤ 1)
#'       \item "Moderate" (2-5)
#'       \item "Severe" (6-9)
#'       \item "Very Severe" (≥ 10)
#'     }
#' }
#'
#' @examples
#' # Example data
#' example_data <- tibble::tibble(
#'   frequency = c(0, 2, 3, 4, 1),
#'   amount = c(1, 2, 3, 3, 2)
#' )
#'
#' # Compute Sandvik Severity Index
#' sandvik_severity_index(example_data$frequency, example_data$amount)
#'
#' @references
#' Sandvik, H., Seim, A., Vanvik, A., Hunskaar, S. (2000). A severity index for
#' epidemiological surveys of female urinary incontinence: Comparison with 48-hour
#' pad-weighing tests. \emph{Neurourology and Urodynamics, 19}(2), 137-145.
#'
#' @export
sandvik_severity_index <- function(frequency, amount) {
  # Validate inputs
  assertthat::assert_that(
    is.numeric(frequency), all(frequency %in% 0:4),
    msg = "Frequency must be a numeric value between 0 and 4."
  )

  assertthat::assert_that(
    is.numeric(amount), all(amount %in% 1:3),
    msg = "Amount must be a numeric value between 1 and 3."
  )

  # Compute Sandvik Severity Index
  severity_index <- frequency * amount

  # Classify severity
  severity_level <- dplyr::case_when(
    severity_index <= 1 ~ "Slight",
    severity_index <= 5 ~ "Moderate",
    severity_index <= 9 ~ "Severe",
    severity_index >= 10 ~ "Very Severe",
    TRUE ~ "Unknown"
  )

  # Return a tibble with results
  result_df <- tibble::tibble(
    Frequency = frequency,
    Amount = amount,
    Severity_Index = severity_index,
    Severity_Level = severity_level
  )

  return(result_df)
}
