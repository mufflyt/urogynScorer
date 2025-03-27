utils::globalVariables(c("response", "non_missing", "mean_response", "pfdi_total_score"))

#' Calculate PFDI Scores from Minimal Dataset
#'
#' Calculates Pelvic Floor Distress Inventory (PFDI-20) total score and subscale scores
#' (POPDI-6, CRADI-8, and UDI-6) from raw questionnaire data.
#'
#' @param patient_data Deprecated. Use input_data instead. A data frame containing PFDI questionnaire responses.
#' @param input_data A data frame containing PFDI questionnaire responses.
#' @param id_column Character string specifying the column name containing patient
#'        identifiers. Default is "Record_ID".
#' @param missing_threshold Numeric value between 0 and 1 specifying the maximum
#'        proportion of missing values allowed per subscale. Default is 0.5 (50%).
#' @param verbose Logical indicating whether to print detailed logging messages.
#'        Default is FALSE.
#' @param output_file Character string specifying the path to save the results.
#'        If NULL (default), results are not saved to a file.
#' @param scale_factor Numeric. The scaling factor to apply to the scores.
#'        Default is 25.
#'
#' @return A tibble containing patient IDs and calculated scores for PFDI-20 total
#'         and the three subscales (POPDI-6, CRADI-8, and UDI-6).
#'
#' @details
#' The PFDI-20 consists of three subscales: POPDI-6 (questions 1-6), CRADI-8
#' (questions 7-14), and UDI-6 (questions 15-20). Each question uses a 0-4 scale.
#' The function calculates the mean score for each subscale, multiplies by the scale
#' factor (default: 25) to get a 0-100 scale, and sums the three subscale scores to
#' get the PFDI-20 total (0-300 scale).
#'
#' The function supports multiple column naming conventions including standard format
#' (popdi_q*, cradi_q*, udi_q*), numeric format (PFDI-1 through PFDI-20), subscale-specific
#' format (PFDI-POPDI-*, etc.), and dot notation (PFDI.1, etc.).
#'
#' @examples
#' # Example 1: Basic usage with standard column names
#' patient_data <- data.frame(
#'   Record_ID = 1,
#'   popdi_q1 = 2, popdi_q2 = 1, popdi_q3 = 0,
#'   cradi_q1 = 1, cradi_q2 = 0, cradi_q3 = 2,
#'   udi_q1 = 3, udi_q2 = 1, udi_q3 = 0
#' )
#' pfdi_scores <- calculate_pfdi_scores(
#'   input_data = patient_data,
#'   scale_factor = 25,
#'   verbose = TRUE
#' )
#'
#' # Example 2: With alternative column names
#' alt_data <- data.frame(
#'   Patient_ID = "PT001",
#'   "PFDI-1" = 2, "PFDI-2" = 1, "PFDI-3" = 0,
#'   "PFDI-7" = 1, "PFDI-8" = 0, "PFDI-9" = 2,
#'   "PFDI-15" = 3, "PFDI-16" = 1, "PFDI-17" = 0
#' )
#' pfdi_scores <- calculate_pfdi_scores(
#'   input_data = alt_data,
#'   id_column = "Patient_ID",
#'   missing_threshold = 0.5
#' )
#'
#' # Example 3: With output file
#' temp_file <- tempfile(fileext = ".csv")
#' pfdi_scores <- calculate_pfdi_scores(
#'   input_data = patient_data,
#'   output_file = temp_file
#' )
#'
#' @importFrom assertthat assert_that
#' @importFrom logger log_info log_debug log_error log_warn log_threshold
#' @importFrom dplyr mutate select filter
#' @importFrom tibble tibble as_tibble
#' @importFrom utils write.csv
#' @importFrom rlang .data
#'
#' @export
calculate_pfdi_scores <- function(input_data = NULL,
                                  id_column = "Record_ID",
                                  missing_threshold = 0.5,
                                  verbose = FALSE,
                                  output_file = NULL,
                                  scale_factor = 25,
                                  patient_data = NULL) {
  # For backward compatibility with tests still using patient_data parameter
  if (!is.null(patient_data)) {
    input_data <- patient_data
    if (verbose) logger::log_warn("'patient_data' parameter is deprecated, please use 'input_data' instead")
  }

  # Make sure we have input data from one of the parameters
  if (is.null(input_data)) {
    stop("One of 'input_data' or 'patient_data' must be provided")
  }
  # Setup logging based on verbose parameter
  if (verbose) {
    logger::log_threshold(logger::DEBUG)
    logger::log_info("Starting PFDI score calculation")
    logger::log_debug(paste0("Input parameters: id_column = ", id_column,
                             ", missing_threshold = ", missing_threshold,
                             ", scale_factor = ", scale_factor))
  }

  # Validate inputs
  tryCatch({
    validate_pfdi_inputs(input_data, id_column, missing_threshold, scale_factor)

    if (verbose) {
      logger::log_debug(paste0("Input data dimensions: ", nrow(input_data), " rows, ",
                               ncol(input_data), " columns"))
      logger::log_debug(paste0("Input data column names: ",
                               paste(colnames(input_data), collapse = ", ")))
    }

    # Identify domain questions
    if (verbose) logger::log_debug("Identifying PFDI question columns")
    pfdi_columns <- identify_pfdi_columns(input_data, verbose)

    # Calculate subscale scores
    if (verbose) logger::log_debug("Calculating subscale scores")
    subscale_scores <- calculate_pfdi_subscales(
      input_data = input_data,
      id_column = id_column,
      pfdi_columns = pfdi_columns,
      missing_threshold = missing_threshold,
      scale_factor = scale_factor,
      verbose = verbose
    )

    # Calculate total PFDI score
    if (verbose) logger::log_debug("Calculating total PFDI score")
    final_scores <- calculate_pfdi_total(subscale_scores, verbose)

    # Save to file if specified
    if (!is.null(output_file)) {
      if (verbose) logger::log_info(paste0("Saving results to file: ", output_file))
      utils::write.csv(final_scores, output_file, row.names = FALSE)
    }

    if (verbose) {
      logger::log_info("PFDI score calculation completed successfully")
      logger::log_debug(paste0("Output data dimensions: ", nrow(final_scores), " rows, ",
                               ncol(final_scores), " columns"))
    }

    return(tibble::as_tibble(final_scores))
  },
  error = function(err) {
    logger::log_error(paste0("Error in PFDI score calculation: ", err$message))
    stop(paste0("Failed to calculate PFDI scores: ", err$message))
  })
}

#' Validate input parameters for PFDI score calculation
#'
#' @param input_data Data frame containing PFDI responses
#' @param id_column Column name for patient identifiers
#' @param missing_threshold Maximum allowed proportion of missing values per subscale
#' @param scale_factor The scaling factor to apply to scores
#'
#' @return NULL invisibly
#' @noRd
validate_pfdi_inputs <- function(input_data, id_column, missing_threshold, scale_factor) {
  # Check if input_data is a data frame
  assertthat::assert_that(
    is.data.frame(input_data),
    msg = "input_data must be a data frame"
  )

  # Check if id_column exists in input_data
  assertthat::assert_that(
    id_column %in% colnames(input_data),
    msg = paste0("id_column '", id_column, "' not found in input_data")
  )

  # Check if missing_threshold is between 0 and 1
  assertthat::assert_that(
    is.numeric(missing_threshold) && missing_threshold >= 0 && missing_threshold <= 1,
    msg = "missing_threshold must be between 0 and 1"
  )

  # Check if scale_factor is numeric and positive
  assertthat::assert_that(
    is.numeric(scale_factor) && scale_factor > 0,
    msg = "scale_factor must be a positive number"
  )

  return(invisible(NULL))
}

#' Identify PFDI question columns in various formats
#'
#' @param input_data Data frame containing PFDI responses
#' @param verbose Whether to print detailed logging messages
#'
#' @return A list with vectors of column names for each subscale
#' @noRd
identify_pfdi_columns <- function(input_data, verbose = FALSE) {
  # Check for standard format (popdi_q*, cradi_q*, udi_q*)
  popdi_cols <- grep("^popdi_q\\d+", names(input_data), value = TRUE)
  cradi_cols <- grep("^cradi_q\\d+", names(input_data), value = TRUE)
  udi_cols <- grep("^udi_q\\d+", names(input_data), value = TRUE)

  if (verbose) {
    logger::log_debug(paste0("Found ", length(popdi_cols), " standard POPDI questions"))
    logger::log_debug(paste0("Found ", length(cradi_cols), " standard CRADI questions"))
    logger::log_debug(paste0("Found ", length(udi_cols), " standard UDI questions"))
  }

  # If standard format not found, check for PFDI-POPDI-*, etc.
  if (length(c(popdi_cols, cradi_cols, udi_cols)) == 0) {
    popdi_cols <- grep("^PFDI-POPDI-\\d+", names(input_data), value = TRUE)
    cradi_cols <- grep("^PFDI-CRADI-\\d+", names(input_data), value = TRUE)
    udi_cols <- grep("^PFDI-UDI-\\d+", names(input_data), value = TRUE)

    if (verbose) {
      logger::log_debug(paste0("Found ", length(popdi_cols), " subscale-labeled POPDI questions"))
      logger::log_debug(paste0("Found ", length(cradi_cols), " subscale-labeled CRADI questions"))
      logger::log_debug(paste0("Found ", length(udi_cols), " subscale-labeled UDI questions"))
    }
  }

  # If still not found, check for numbered PFDI-* format and map to subscales
  if (length(c(popdi_cols, cradi_cols, udi_cols)) == 0) {
    pfdi_cols <- grep("^PFDI-\\d+$", names(input_data), value = TRUE)

    if (length(pfdi_cols) > 0) {
      if (verbose) logger::log_debug(paste0("Found ", length(pfdi_cols), " numbered PFDI questions"))

      # Extract numeric part and convert to integer
      pfdi_nums <- as.integer(gsub("^PFDI-(\\d+)$", "\\1", pfdi_cols))

      # Map to subscales based on question numbers
      popdi_cols <- pfdi_cols[pfdi_nums >= 1 & pfdi_nums <= 6]
      cradi_cols <- pfdi_cols[pfdi_nums >= 7 & pfdi_nums <= 14]
      udi_cols <- pfdi_cols[pfdi_nums >= 15 & pfdi_nums <= 20]

      if (verbose) {
        logger::log_debug(paste0("Mapped ", length(popdi_cols), " POPDI questions"))
        logger::log_debug(paste0("Mapped ", length(cradi_cols), " CRADI questions"))
        logger::log_debug(paste0("Mapped ", length(udi_cols), " UDI questions"))
      }
    }
  }

  # If still not found, try the format from the test data (PFDI.1, etc.)
  if (length(c(popdi_cols, cradi_cols, udi_cols)) == 0) {
    pfdi_cols <- grep("^PFDI\\.\\d+$", names(input_data), value = TRUE)

    if (length(pfdi_cols) > 0) {
      if (verbose) logger::log_debug(paste0("Found ", length(pfdi_cols), " dot-notation PFDI questions"))

      # Extract numeric part and convert to integer
      pfdi_nums <- as.integer(gsub("^PFDI\\.(\\d+)$", "\\1", pfdi_cols))

      # Map to subscales based on question numbers
      popdi_cols <- pfdi_cols[pfdi_nums >= 1 & pfdi_nums <= 6]
      cradi_cols <- pfdi_cols[pfdi_nums >= 7 & pfdi_nums <= 14]
      udi_cols <- pfdi_cols[pfdi_nums >= 15 & pfdi_nums <= 20]

      if (verbose) {
        logger::log_debug(paste0("Mapped ", length(popdi_cols), " POPDI questions"))
        logger::log_debug(paste0("Mapped ", length(cradi_cols), " CRADI questions"))
        logger::log_debug(paste0("Mapped ", length(udi_cols), " UDI questions"))
      }
    }
  }

  # Validate that we found PFDI columns
  total_cols <- length(c(popdi_cols, cradi_cols, udi_cols))
  if (total_cols == 0) {
    stop(paste0("No PFDI question columns found in the input data. ",
                "Column names should include 'popdi_q', 'cradi_q', or 'udi_q' prefixes, ",
                "or follow PFDI-n, PFDI.n, or PFDI-SUBSCALE-n patterns."))
  }

  if (verbose) logger::log_debug(paste0("Total PFDI columns identified: ", total_cols))

  # Return the domain questions
  return(list(
    popdi = popdi_cols,
    cradi = cradi_cols,
    udi = udi_cols
  ))
}

#' Calculate PFDI subscale scores
#'
#' @param input_data Data frame containing PFDI responses
#' @param id_column Column name for patient identifiers
#' @param pfdi_columns List of column names for each subscale
#' @param missing_threshold Maximum allowed proportion of missing values per subscale
#' @param scale_factor The scaling factor to apply to scores
#' @param verbose Whether to print detailed logging messages
#'
#' @return A data frame with subscale scores
#' @noRd
calculate_pfdi_subscales <- function(input_data, id_column, pfdi_columns,
                                     missing_threshold, scale_factor, verbose = FALSE) {
  # Initialize results data frame with ID
  subscale_scores <- input_data[, id_column, drop = FALSE]

  # Calculate POPDI score
  if (length(pfdi_columns$popdi) > 0) {
    if (verbose) {
      logger::log_debug(paste0("Calculating POPDI_6 score from ",
                               length(pfdi_columns$popdi), " questions"))
    }

    subscale_scores$POPDI_6 <- calculate_subscale_score(
      input_data,
      pfdi_columns$popdi,
      missing_threshold,
      scale_factor,
      verbose
    )

    if (verbose) logger::log_debug("POPDI_6 score calculation complete")
  } else {
    subscale_scores$POPDI_6 <- NA_real_
    if (verbose) logger::log_warn("No POPDI columns found, score set to NA")
  }

  # Calculate CRADI score
  if (length(pfdi_columns$cradi) > 0) {
    if (verbose) {
      logger::log_debug(paste0("Calculating CRADI_8 score from ",
                               length(pfdi_columns$cradi), " questions"))
    }

    subscale_scores$CRADI_8 <- calculate_subscale_score(
      input_data,
      pfdi_columns$cradi,
      missing_threshold,
      scale_factor,
      verbose
    )

    if (verbose) logger::log_debug("CRADI_8 score calculation complete")
  } else {
    subscale_scores$CRADI_8 <- NA_real_
    if (verbose) logger::log_warn("No CRADI columns found, score set to NA")
  }

  # Calculate UDI score
  if (length(pfdi_columns$udi) > 0) {
    if (verbose) {
      logger::log_debug(paste0("Calculating UDI_6 score from ",
                               length(pfdi_columns$udi), " questions"))
    }

    subscale_scores$UDI_6 <- calculate_subscale_score(
      input_data,
      pfdi_columns$udi,
      missing_threshold,
      scale_factor,
      verbose
    )

    if (verbose) logger::log_debug("UDI_6 score calculation complete")
  } else {
    subscale_scores$UDI_6 <- NA_real_
    if (verbose) logger::log_warn("No UDI columns found, score set to NA")
  }

  return(subscale_scores)
}

#' Calculate a single subscale score with missing data handling
#'
#' @param input_data Data frame containing PFDI responses
#' @param columns Column names for this subscale
#' @param missing_threshold Maximum allowed proportion of missing values
#' @param scale_factor The scaling factor to apply to scores
#' @param verbose Whether to print detailed logging messages
#'
#' @return A vector of subscale scores
#' @noRd
calculate_subscale_score <- function(input_data, columns, missing_threshold,
                                     scale_factor, verbose = FALSE) {
  # Extract subscale data
  subscale_data <- input_data[, columns, drop = FALSE]

  # Count non-missing responses per row
  n_questions <- length(columns)
  non_missing_counts <- rowSums(!is.na(subscale_data))

  # Calculate required number of responses
  min_required <- ceiling(n_questions * (1 - missing_threshold))
  if (verbose) {
    logger::log_debug(paste0("Minimum required responses: ", min_required,
                             " out of ", n_questions, " questions"))
  }

  # Calculate mean for each row, handling missing threshold
  scores <- rep(NA_real_, nrow(input_data))
  valid_rows <- non_missing_counts >= min_required

  if (any(valid_rows)) {
    # Use base::rowMeans (not dplyr::rowMeans) with na.rm=TRUE for valid rows
    means <- rowMeans(subscale_data[valid_rows, , drop = FALSE], na.rm = TRUE)
    scores[valid_rows] <- means * scale_factor

    if (verbose) {
      logger::log_debug(paste0("Calculated scores for ", sum(valid_rows),
                               " rows with sufficient data"))
    }
  } else if (verbose) {
    logger::log_warn("No rows had sufficient data to calculate this subscale score")
  }

  # Round to 1 decimal place
  scores <- round(scores, 1)

  return(scores)
}

#' Calculate total PFDI score from subscales
#'
#' @param subscale_scores Data frame containing subscale scores
#' @param verbose Whether to print detailed logging messages
#'
#' @return A data frame with total PFDI score added
#' @noRd
calculate_pfdi_total <- function(subscale_scores, verbose = FALSE) {
  # Identify subscale columns
  subscale_cols <- c("POPDI_6", "CRADI_8", "UDI_6")

  # Calculate total PFDI score (sum of subscales)
  subscale_scores$PFDI_20_total <- rowSums(
    subscale_scores[, subscale_cols, drop = FALSE],
    na.rm = FALSE  # If any subscale is NA, total should be NA
  )

  if (verbose) {
    n_valid <- sum(!is.na(subscale_scores$PFDI_20_total))
    logger::log_debug(paste0("Calculated total PFDI score for ", n_valid, " rows"))
  }

  return(subscale_scores)
}
