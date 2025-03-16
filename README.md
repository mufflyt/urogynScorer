urogynScorer: Urogynecology Questionnaire Scoring Package
================
Tyler Muffly, MD
2025-03-15

<!-- badges: start -->

![R-CMD-check](https://github.com/mufflyt/urogynScorer/workflows/R-CMD-check/badge.svg)
![CRAN status](https://www.r-pkg.org/badges/version/urogynScorer)
![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
[![DOI](https://zenodo.org/badge/DOI/zenodo.placeholder.svg)](https://zenodo.org/placeholder)
<!-- badges: end -->

## Overview

The **urogynScorer** package provides functions for scoring validated
**urogynecology patient-reported outcome measures (PROMs)** used in
clinical practice and research. The package implements evidence-based
scoring algorithms for questionnaires including:

- **Pelvic Floor Distress Inventory (PFDI-20)** and its subscales
  (POPDI-6, CRADI-8, and UDI-6)
- **Patient Global Impression of Improvement (PGI-I)**
- **Sandvik Severity Index** for urinary incontinence

## Key Features

- **Automated, standardized scoring** of validated questionnaires
- **Robust handling of missing data** following instrument-specific
  guidelines
- **Comprehensive input validation** with informative error messages
- **Detailed logging** capabilities for debugging and quality assurance
- **Tidy output** compatible with the tidyverse ecosystem
- **Extensive documentation** with examples and references to primary
  literature

## Installation

### CRAN (Recommended)

``` r
install.packages("urogynScorer")
```

### Development Version

``` r
# Install devtools if you haven't already
install.packages("devtools")

# Install the development version from GitHub
devtools::install_github("mufflyt/urogynScorer")
```

## Usage

### Load the Package

``` r
library(urogynScorer)
library(dplyr)  # For data manipulation
```

### PFDI-20 Scoring Example

The PFDI-20 measures pelvic floor symptoms across three domains: Pelvic
Organ Prolapse (POPDI-6), Colorectal-Anal (CRADI-8), and Urinary
(UDI-6).

``` r
# Load sample data
data("pfdi_data")

# View first few rows
head(pfdi_data)

# Score PFDI-20
pfdi_results <- calculate_pfdi_scores(
  patient_data = pfdi_data,
  id_column = "Record_ID",
  missing_threshold = 0.5,  # Allow up to 50% missing data per subscale
  verbose = TRUE  # Enable detailed logging
)

# View results
pfdi_results
```

### Patient Global Impression of Improvement (PGI-I) Example

The PGI-I assesses patient-reported improvement following treatment on a
7-point scale.

``` r
# Create sample data
pgii_data <- data.frame(
  Record_ID = c("PT001", "PT002", "PT003", "PT004", "PT005"),
  pgii_response = c(1, 2, 3, 4, NA)  # 1=Very much better, 7=Very much worse
)

# Score PGI-I responses
pgii_results <- score_pgii(
  patient_data = pgii_data,
  item_name = "pgii_response",
  keep_n_valid = TRUE,
  verbose = TRUE
)

# View results
pgii_results
```

### Sandvik Severity Index Example

The Sandvik index categorizes urinary incontinence severity based on
frequency and amount of leakage.

``` r
# Create sample data
sandvik_data <- tibble::tibble(
  frequency = c(1, 2, 3, 4, 0),  # 0=Never to 4=Daily
  amount = c(1, 2, 3, 2, 1)      # 1=Drops to 3=Large amounts
)

# Calculate Sandvik Severity Index
sandvik_results <- sandvik_severity_index(
  frequency = sandvik_data$frequency,
  amount = sandvik_data$amount
)

# View results
sandvik_results
```

## Full Documentation

For detailed documentation, please see the package vignettes:

``` r
# List available vignettes
vignette(package = "urogynScorer")

# View PFDI-20 vignette
vignette("Scoring_the_Pelvic_Floor_Distress_Inventory", package = "urogynScorer")
```

## How Questionnaires Are Scored

### PFDI-20 Scoring

The PFDI-20 consists of three subscales: - **POPDI-6** (questions 1-6):
Pelvic Organ Prolapse Distress Inventory - **CRADI-8** (questions 7-14):
Colorectal-Anal Distress Inventory - **UDI-6** (questions 15-20):
Urinary Distress Inventory

Each question uses a 0-4 scale: - 0: Not at all - 1: Somewhat - 2:
Moderately - 3: Quite a bit - 4: Extremely

Scoring: 1. Calculate the mean score for each subscale (if ≥50% of items
are answered) 2. Multiply the mean by 25 to get a 0-100 scale 3. Sum the
three subscale scores to get the PFDI-20 total (0-300 scale)

### PGI-I Scoring

The PGI-I is scored from 1-7: 1. Very much better 2. Much better 3. A
little better 4. No change 5. A little worse 6. Much worse 7. Very much
worse

The `score_pgii()` function also adds a binary improvement indicator
(1=improved \[1-3\], 0=not improved \[4-7\]).

### Sandvik Severity Index

The index is calculated by multiplying frequency (0-4) by amount (1-3)
of leakage, resulting in: - **Slight** (≤1) - **Moderate** (2-5) -
**Severe** (6-9) - **Very Severe** (≥10)

## Citation

If you use urogynScorer in your research, please cite:

    Muffly T (2025). urogynScorer: Calculate Scores for Urogynecology Questionnaires. 
    R package version 0.1.0. https://github.com/mufflyt/urogynScorer

## References

- **Barber, M. D., Walters, M. D., & Bump, R. C.** (2005). Short forms
  of two condition-specific quality-of-life questionnaires for women
  with pelvic floor disorders (PFDI-20 and PFIQ-7). *American Journal of
  Obstetrics and Gynecology, 193*(1), 103-113.

- **Yalcin, I., & Bump, R. C.** (2003). Validation of two global
  impression questionnaires for incontinence. *American Journal of
  Obstetrics and Gynecology, 189*(1), 98-101.

- **Sandvik, H., Seim, A., Vanvik, A., & Hunskaar, S.** (2000). A
  severity index for epidemiological surveys of female urinary
  incontinence: Comparison with 48-hour pad-weighing tests.
  *Neurourology and Urodynamics, 19*(2), 137-145.

## Contributing

Contributions to urogynScorer are welcome! Please submit issues and pull
requests on GitHub.

License: MIT + file LICENSE
