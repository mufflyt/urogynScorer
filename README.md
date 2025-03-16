urogynScorer: Urogynecology Questionnaire Scoring Package
================
Your Name or Organization
2025-03-15

# urogynScorer

<!-- badges: start -->

![CRAN status](https://www.r-pkg.org/badges/version/urogynScorer)
![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
<!-- badges: end -->

The **urogynScorer** package provides functions for scoring validated
**urogynecology patient-reported outcome (PRO) measures**, including the
**Pelvic Floor Distress Inventory (PFDI-20)**, **Pelvic Organ
Prolapse/Urinary Incontinence Sexual Questionnaire (PISQ)**, **Sandvik
Severity Index**, and the **Patient Global Impression of Improvement
(PGI-I)**. These tools are widely used in clinical and research settings
to assess patient-reported outcomes in pelvic floor disorders.

## Features

- **Automated scoring** for validated urogynecology PROs
- **Handles missing data** following instrument-specific guidelines
- **Provides clinically meaningful summary variables**
- **Designed for reproducible research** with robust error handling

## Installation

You can install the **development version** of `urogynScorer` from
GitHub using:

``` r
# Install devtools if you haven't already
install.packages("devtools")

# Install urogynScorer from GitHub
devtools::install_github("your-username/urogynScorer")
```

## Usage

### Load the Package

``` r
library(urogynScorer)
```

### **Example 1: Scoring the PGI-I**

The **PGI-I** is a single-item questionnaire assessing perceived
improvement after treatment.

``` r
# Example dataset
df <- data.frame(pgii_response = c(1, 2, 3, 4, 5, 6, 7, NA))

# Score PGI-I
score_pgii(df, "pgii_response")
```

### **Example 2: Scoring the PFDI-20**

The **Pelvic Floor Distress Inventory (PFDI-20)** assesses pelvic floor
dysfunction symptoms across three subscales.

``` r
# Example dataset with PFDI-20 responses
df_pfdi <- data.frame(
  pfdi1 = c(3, 4, 2, 1, NA),
  pfdi2 = c(5, 2, 1, 3, 4),
  pfdi3 = c(NA, 1, 3, 2, 5)
)

# Score PFDI-20
score_pfdi(df_pfdi, item_prefix = "pfdi")
```

### **Example 3: Scoring the Sandvik Severity Scale**

The **Sandvik Incontinence Severity Index** categorizes the severity of
urinary incontinence based on frequency and amount.

``` r
# Example dataset with incontinence severity responses
df_sandvik <- data.frame(
  frequency = c(1, 2, 3, 4, 2),
  amount = c(1, 3, 2, 2, 1)
)

# Score Sandvik Severity Index
score_sandvik(df_sandvik, freq_col = "frequency", amount_col = "amount")
```

## Supported Questionnaires

| Questionnaire | Description |
|----|----|
| **PGI-I** | Patient Global Impression of Improvement |
| **PFDI-20** | Pelvic Floor Distress Inventory |
| **PISQ** | Pelvic Organ Prolapse/Urinary Incontinence Sexual Questionnaire |
| **Sandvik Severity Scale** | Urinary incontinence severity index |

## Handling Missing Data

Each function follows the **validated missing data rules** for the
respective questionnaire. If a respondent skips certain items, the
function prorates scores accordingly.

## References

- **Yalcin, I., & Bump, R. C.** (2003). Validation of two global
  impression questionnaires for incontinence. *American Journal of
  Obstetrics and Gynecology, 189*(1), 98-101.
- **Rogers, R. G.** (2006). Validation of the PISQ-12 questionnaire.
  *International Urogynecology Journal, 17*(6), 636-645.
- **Sandvik, H., et al.** (1993). Severity index for epidemiological
  surveys of female urinary incontinence. *Journal of Clinical
  Epidemiology, 46*(1), 11-19.

## Contributing

We welcome contributions! If you find a bug, have a feature request, or
want to add additional urogynecology PRO measures, please submit an
issue or pull request on GitHub.

## License

This package is released under the **MIT License**.
