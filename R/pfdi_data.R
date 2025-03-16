#' Pelvic Floor Distress Inventory (PFDI-20) Dataset
#'
#' This dataset contains responses to the **Pelvic Floor Distress Inventory (PFDI-20)**,
#' a validated questionnaire used in urogynecology to assess symptoms related to pelvic
#' organ prolapse, colorectal distress, and urinary distress. It includes responses
#' from a urogynecology patient cohort.
#'
#' @format A data frame with `n` rows and multiple columns:
#' \describe{
#'   \item{Record_ID}{Unique identifier for each respondent.}
#'   \item{PFDI-1 to PFDI-20}{Responses to the 20 items in the PFDI-20 questionnaire.}
#'   \item{PFDI-POPDI-1 to PFDI-POPDI-6}{Items corresponding to the Pelvic Organ Prolapse Distress Inventory (POPDI-6).}
#'   \item{PFDI-CRADI-1 to PFDI-CRADI-8}{Items corresponding to the Colorectal-Anal Distress Inventory (CRADI-8).}
#'   \item{PFDI-UDI-1 to PFDI-UDI-6}{Items corresponding to the Urinary Distress Inventory (UDI-6).}
#' }
#'
#' @details
#' The **Pelvic Floor Distress Inventory (PFDI-20)** is composed of three subscales:
#' - **POPDI-6**: Pelvic Organ Prolapse Distress Inventory (6 items)
#' - **CRADI-8**: Colorectal-Anal Distress Inventory (8 items)
#' - **UDI-6**: Urinary Distress Inventory (6 items)
#'
#' Each item is scored on a **Likert scale from 0 to 4**, where:
#' - 0 = Not at all
#' - 1 = Somewhat
#' - 2 = Moderately
#' - 3 = Quite a bit
#' - 4 = Greatly
#'
#' The final score is computed using the **score_pfdi()** function in the `urogynScorer` package.
#'
#' @usage
#' data("pfdi_data")
#'
#' @source Barber, M. D., Walters, M. D., & Bump, R. C. (2005).
#' Short forms of two condition-specific quality-of-life questionnaires for
#' women with pelvic floor disorders (PFDI-20 and PFIQ-7).
#' *American Journal of Obstetrics and Gynecology*, 193(1), 103-113.
#'
#' @examples
#' data("pfdi_data")
#' head(pfdi_data)
#'
"pfdi_data"
