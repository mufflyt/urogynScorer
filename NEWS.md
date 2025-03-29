# urogynScorer 0.2.0

## New features
* Added comprehensive validation and error handling for all functions
* Added detailed logging capabilities using the logger package
* Added support for alternative column naming conventions in `calculate_pfdi_scores()`
* Added robust handling of missing data with configurable thresholds
* Added normalized scoring output (0-100 scale) for all metrics
* Added package hex sticker logo

## Bug fixes
* Fixed subscale calculation in `calculate_pfdi_scores()` to correctly use scale of 25 points per question
* Fixed issue with missing value handling in `score_pgii()`
* Fixed edge case handling in `sandvik_severity_index()`

## Documentation
* Added comprehensive roxygen documentation with expanded examples
* Added parameter validation details and error messages
* Added vignette for scoring the Pelvic Floor Distress Inventory (PFDI-20)
* Added references to original validation studies for all instruments
* Added README with hex sticker logo and examples for all functions

## Other
* Initial CRAN submission
* Added unit tests with testthat (>= 3.0.0)
* Implemented CI/CD pipeline with GitHub Actions
