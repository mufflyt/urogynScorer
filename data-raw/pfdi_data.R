#' Generate simulated PFDI-20 data
#'
#' @param n Number of patients to simulate
#' @return A data frame with simulated PFDI-20 responses
#' @noRd
generate_pfdi_data <- function(n = 20) {
  set.seed(1978) # For reproducibility

  # Create patient IDs
  patient_id <- paste0("P", sprintf("%03d", 1:n))

  # Generate random responses (0-4) for each PFDI-20 item
  # With some NAs occasionally to test NA handling
  generate_responses <- function(n) {
    responses <- sample(0:4, n, replace = TRUE)
    # Randomly insert some NAs (about 5% of values)
    na_indices <- sample(1:n, size = round(n * 0.05), replace = FALSE)
    if (length(na_indices) > 0) {
      responses[na_indices] <- NA
    }
    return(responses)
  }

  # Create the data frame
  pfdi_data <- data.frame(
    patient_id = patient_id,

    # POPDI-6 items
    "PFDI-01-POPDI" = generate_responses(n),
    "PFDI-02-POPDI" = generate_responses(n),
    "PFDI-03-POPDI" = generate_responses(n),
    "PFDI-04-POPDI" = generate_responses(n),
    "PFDI-05-POPDI" = generate_responses(n),
    "PFDI-06-POPDI" = generate_responses(n),

    # CRADI-8 items
    "PFDI-07-CRADI" = generate_responses(n),
    "PFDI-08-CRADI" = generate_responses(n),
    "PFDI-09-CRADI" = generate_responses(n),
    "PFDI-10-CRADI" = generate_responses(n),
    "PFDI-11-CRADI" = generate_responses(n),
    "PFDI-12-CRADI" = generate_responses(n),
    "PFDI-13-CRADI" = generate_responses(n),
    "PFDI-14-CRADI" = generate_responses(n),

    # UDI-6 items
    "PFDI-15-UDI" = generate_responses(n),
    "PFDI-16-UDI" = generate_responses(n),
    "PFDI-17-UDI" = generate_responses(n),
    "PFDI-18-UDI" = generate_responses(n),
    "PFDI-19-UDI" = generate_responses(n),
    "PFDI-20-UDI" = generate_responses(n)
  )

  return(pfdi_data)
}

# Generate the dataset
pfdi_data <- generate_pfdi_data()

# Save the data
usethis::use_data(pfdi_data, overwrite = TRUE)
