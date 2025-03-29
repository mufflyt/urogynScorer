#' @keywords internal
"_PACKAGE"

#' urogynScorer: Calculate Scores for Urogynecology Questionnaires
#'
#' @description
#' A comprehensive toolkit for scoring, validating, and analyzing patient-reported
#' outcome measures (PROMs) used in urogynecology and pelvic floor medicine. The
#' package implements standardized scoring algorithms with robust validation for
#' commonly used questionnaires in clinical practice and research.
#'
#' @details
#' The package provides functions for scoring:
#'
#' \itemize{
#'   \item \strong{Pelvic Floor Distress Inventory (PFDI-20)} - Including its subscales:
#'     \itemize{
#'       \item Pelvic Organ Prolapse Distress Inventory (POPDI-6)
#'       \item Colorectal-Anal Distress Inventory (CRADI-8)
#'       \item Urinary Distress Inventory (UDI-6)
#'     }
#'   \item \strong{Patient Global Impression of Improvement (PGI-I)} - A single-item
#'     measure of subjective treatment efficacy
#'   \item \strong{Sandvik Severity Index} - For assessing urinary incontinence severity
#' }
#'
#' Key features include:
#' \itemize{
#'   \item Evidence-based handling of missing data
#'   \item Robust validation of input data
#'   \item Detailed logging capabilities
#'   \item Comprehensive documentation with clinical context
#'   \item Support for multiple column naming conventions
#' }
#'
#' @references
#' \itemize{
#'   \item Barber MD, Walters MD, Bump RC (2005). Short forms of two condition-specific
#'     quality-of-life questionnaires for women with pelvic floor disorders (PFDI-20 and PFIQ-7).
#'     \emph{American Journal of Obstetrics and Gynecology, 193}(1), 103-113.
#'   \item Yalcin I, Bump RC (2003). Validation of two global impression questionnaires
#'     for incontinence. \emph{American Journal of Obstetrics and Gynecology, 189}(1), 98-101.
#'   \item Sandvik H, Seim A, Vanvik A, Hunskaar S (2000). A severity index for
#'     epidemiological surveys of female urinary incontinence: Comparison with 48-hour
#'     pad-weighing tests. \emph{Neurourology and Urodynamics, 19}(2), 137-145.
#'   \item Jelovsek JE, Barber MD (2006). Patient-reported outcome measures in pelvic
#'     floor disorders. \emph{American Journal of Obstetrics and Gynecology, 194}(5), 1455-1461.
#' }
#'
#' @name urogynScorer-package
NULL
