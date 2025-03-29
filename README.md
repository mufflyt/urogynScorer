urogynScorer: Urogynecology Questionnaire Scoring Package
================
Tyler Muffly, MD
2025-03-28

<!-- README.md is generated from README.Rmd. Please edit that file -->

# <a href="https://mufflyt.github.io/urogynScorer/"><img src="man/figures/logo.png" align="right" height="240" alt="urogynScorer website" /></a>

<!-- badges: start -->

![R-CMD-check](https://github.com/mufflyt/urogynScorer/workflows/R-CMD-check/badge.svg)
![CRAN status](https://www.r-pkg.org/badges/version/urogynScorer)
![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)
[![Codecov test
coverage](https://codecov.io/gh/mufflyt/urogynScorer/graph/badge.svg)](https://app.codecov.io/gh/mufflyt/urogynScorer)
<!-- badges: end -->

## Overview

**urogynScorer** provides functions for scoring validated
patient-reported outcome measures used in urogynecology clinical
practice and research. The package implements evidence-based scoring
algorithms with comprehensive validation and error handling for:

- **Pelvic Floor Distress Inventory (PFDI-20)** and its subscales:
  - Pelvic Organ Prolapse Distress Inventory (POPDI-6)
  - Colorectal-Anal Distress Inventory (CRADI-8)
  - Urinary Distress Inventory (UDI-6)
- **Patient Global Impression of Improvement (PGI-I)**
- **Sandvik Severity Index** for urinary incontinence

## Installation

### CRAN version (recommended)

``` r
install.packages("urogynScorer")
```

### Development version

``` r
# Install devtools if you haven't already
install.packages("devtools")

# Install the development version from GitHub
devtools::install_github("mufflyt/urogynScorer")
```

## Key Features

- **Automated, standardized scoring** following validated methodologies
- **Robust handling of missing data** following instrument-specific
  guidelines
- **Comprehensive input validation** with informative error messages
- **Detailed logging** capabilities for quality assurance and debugging
- **Elegant handling of alternative column naming conventions**
- **Tidy output** compatible with the tidyverse ecosystem
- **Extensive documentation** with examples and references to primary
  literature

## Usage Examples

### Pelvic Floor Distress Inventory (PFDI-20) Scoring

The Pelvic Floor Distress Inventory (PFDI-20) measures pelvic floor
symptoms across three domains:

``` r
library(urogynScorer)
library(dplyr)

# Load sample data
data("pfdi_data_minimal")

# View first few rows
head(pfdi_data_minimal)

# Score PFDI-20
pfdi_results <- calculate_pfdi_scores(
  patient_data = pfdi_data_minimal,
  id_column = "Record_ID",
  missing_threshold = 0.5,  # Allow up to 50% missing data per subscale
  verbose = TRUE            # Enable detailed logging
)

# View results
pfdi_results
```

### Patient Global Impression of Improvement (PGI-I)

The PGI-I assesses patient-reported improvement following treatment on a
7-point scale:

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

### Sandvik Severity Index

The Sandvik index categorizes urinary incontinence severity based on
frequency and amount of leakage:

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

## How Questionnaires Are Scored

### PFDI-20 Scoring

The PFDI-20 consists of three subscales:

| Subscale    | Questions | Description                              |
|-------------|-----------|------------------------------------------|
| **POPDI-6** | 1-6       | Pelvic Organ Prolapse Distress Inventory |
| **CRADI-8** | 7-14      | Colorectal-Anal Distress Inventory       |
| **UDI-6**   | 15-20     | Urinary Distress Inventory               |

Each question uses a 0-4 scale:

| Score | Description |
|-------|-------------|
| 0     | Not at all  |
| 1     | Somewhat    |
| 2     | Moderately  |
| 3     | Quite a bit |
| 4     | Extremely   |

**Scoring process:** 1. Calculate the mean score for each subscale (if
≥50% of items are answered) 2. Multiply the mean by 25 to get a 0-100
scale 3. Sum the three subscale scores to get the PFDI-20 total (0-300
scale)

### PGI-I Scoring

The PGI-I is scored from 1-7:

| Score | Description      |
|-------|------------------|
| 1     | Very much better |
| 2     | Much better      |
| 3     | A little better  |
| 4     | No change        |
| 5     | A little worse   |
| 6     | Much worse       |
| 7     | Very much worse  |

The `score_pgii()` function also adds a binary improvement indicator
(1=improved \[1-3\], 0=not improved \[4-7\]).

### Sandvik Severity Index

The index is calculated by multiplying frequency (0-4) by amount (1-3)
of leakage, resulting in:

| Severity Level  | Score Range |
|-----------------|-------------|
| **Slight**      | ≤1          |
| **Moderate**    | 2-5         |
| **Severe**      | 6-9         |
| **Very Severe** | ≥10         |

## Mathematical Basis for Scoring

### PFDI-20 Scoring Algorithm

The PFDI-20 consists of three subscales: POPDI-6 (questions 1-6),
CRADI-8 (questions 7-14), and UDI-6 (questions 15-20).

For a given subscale with
![n](https://latex.codecogs.com/png.latex?n "n") questions, where
![q_i](https://latex.codecogs.com/png.latex?q_i "q_i") represents the
response to question ![i](https://latex.codecogs.com/png.latex?i "i")
(ranging from 0 to 4):

1.  Calculate the mean score
    ![\bar{q}](https://latex.codecogs.com/png.latex?%5Cbar%7Bq%7D "\bar{q}")
    for each subscale if at least
    ![p\\](https://latex.codecogs.com/png.latex?p%5C%25 "p\%") of the
    items are answered:

![\bar{q} = \frac{\sum\_{i=1}^{n} q_i}{n\_{\text{valid}}}](https://latex.codecogs.com/png.latex?%5Cbar%7Bq%7D%20%3D%20%5Cfrac%7B%5Csum_%7Bi%3D1%7D%5E%7Bn%7D%20q_i%7D%7Bn_%7B%5Ctext%7Bvalid%7D%7D%7D "\bar{q} = \frac{\sum_{i=1}^{n} q_i}{n_{\text{valid}}}")

where
![n\_{\text{valid}}](https://latex.codecogs.com/png.latex?n_%7B%5Ctext%7Bvalid%7D%7D "n_{\text{valid}}")
is the number of non-missing responses, and
![p](https://latex.codecogs.com/png.latex?p "p") is the threshold (by
default,
![p = 50\\](https://latex.codecogs.com/png.latex?p%20%3D%2050%5C%25 "p = 50\%")).

2.  Scale the mean to get a 0-100 score for each subscale:

![\text{Subscale Score} = 25 \times \bar{q}](https://latex.codecogs.com/png.latex?%5Ctext%7BSubscale%20Score%7D%20%3D%2025%20%5Ctimes%20%5Cbar%7Bq%7D "\text{Subscale Score} = 25 \times \bar{q}")

3.  Calculate the PFDI-20 total score by summing the three subscale
    scores:

![\text{PFDI-20 Total} = \text{POPDI-6} + \text{CRADI-8} + \text{UDI-6}](https://latex.codecogs.com/png.latex?%5Ctext%7BPFDI-20%20Total%7D%20%3D%20%5Ctext%7BPOPDI-6%7D%20%2B%20%5Ctext%7BCRADI-8%7D%20%2B%20%5Ctext%7BUDI-6%7D "\text{PFDI-20 Total} = \text{POPDI-6} + \text{CRADI-8} + \text{UDI-6}")

The resulting PFDI-20 total score ranges from 0 to 300, with higher
scores indicating greater distress.

### PGI-I Scoring

The PGI-I is scored directly on a 7-point scale:

![\text{PGI-I Score} = r](https://latex.codecogs.com/png.latex?%5Ctext%7BPGI-I%20Score%7D%20%3D%20r "\text{PGI-I Score} = r")

where ![r](https://latex.codecogs.com/png.latex?r "r") is the patient’s
response (1-7).

In addition, a binary improvement indicator is calculated:

![\text{Improved} = 
\begin{cases} 
1 & \text{if } r \in \\1, 2, 3\\ \\
0 & \text{if } r \in \\4, 5, 6, 7\\
\end{cases}](https://latex.codecogs.com/png.latex?%5Ctext%7BImproved%7D%20%3D%20%0A%5Cbegin%7Bcases%7D%20%0A1%20%26%20%5Ctext%7Bif%20%7D%20r%20%5Cin%20%5C%7B1%2C%202%2C%203%5C%7D%20%5C%5C%0A0%20%26%20%5Ctext%7Bif%20%7D%20r%20%5Cin%20%5C%7B4%2C%205%2C%206%2C%207%5C%7D%0A%5Cend%7Bcases%7D "\text{Improved} = 
\begin{cases} 
1 & \text{if } r \in \{1, 2, 3\} \\
0 & \text{if } r \in \{4, 5, 6, 7\}
\end{cases}")

### Sandvik Severity Index

The Sandvik Severity Index
(![S](https://latex.codecogs.com/png.latex?S "S")) is calculated by
multiplying the frequency
(![f](https://latex.codecogs.com/png.latex?f "f")) and amount
(![a](https://latex.codecogs.com/png.latex?a "a")) of leakage:

![S = f \times a](https://latex.codecogs.com/png.latex?S%20%3D%20f%20%5Ctimes%20a "S = f \times a")

where: - ![f](https://latex.codecogs.com/png.latex?f "f") ranges from 0
(never) to 4 (daily) - ![a](https://latex.codecogs.com/png.latex?a "a")
ranges from 1 (drops) to 3 (large amounts)

The resulting severity index is categorized as:

![\text{Severity Category} = 
\begin{cases} 
\text{Slight} & \text{if } S \leq 1 \\
\text{Moderate} & \text{if } 2 \leq S \leq 5 \\
\text{Severe} & \text{if } 6 \leq S \leq 9 \\
\text{Very Severe} & \text{if } S \geq 10
\end{cases}](https://latex.codecogs.com/png.latex?%5Ctext%7BSeverity%20Category%7D%20%3D%20%0A%5Cbegin%7Bcases%7D%20%0A%5Ctext%7BSlight%7D%20%26%20%5Ctext%7Bif%20%7D%20S%20%5Cleq%201%20%5C%5C%0A%5Ctext%7BModerate%7D%20%26%20%5Ctext%7Bif%20%7D%202%20%5Cleq%20S%20%5Cleq%205%20%5C%5C%0A%5Ctext%7BSevere%7D%20%26%20%5Ctext%7Bif%20%7D%206%20%5Cleq%20S%20%5Cleq%209%20%5C%5C%0A%5Ctext%7BVery%20Severe%7D%20%26%20%5Ctext%7Bif%20%7D%20S%20%5Cgeq%2010%0A%5Cend%7Bcases%7D "\text{Severity Category} = 
\begin{cases} 
\text{Slight} & \text{if } S \leq 1 \\
\text{Moderate} & \text{if } 2 \leq S \leq 5 \\
\text{Severe} & \text{if } 6 \leq S \leq 9 \\
\text{Very Severe} & \text{if } S \geq 10
\end{cases}")

## Documentation

For detailed documentation, please see the package vignettes:

``` r
# List available vignettes
vignette(package = "urogynScorer")

# View PFDI-20 vignette
vignette("Scoring_the_Pelvic_Floor_Distress_Inventory", package = "urogynScorer")
```

## Citation

If you use urogynScorer in your research, please cite:

    Muffly T (2025). urogynScorer: Calculate Scores for Urogynecology Questionnaires. 
    R package version 0.2.0. https://github.com/mufflyt/urogynScorer

## References

- **Barber MD, Walters MD, Bump RC** (2005). Short forms of two
  condition-specific quality-of-life questionnaires for women with
  pelvic floor disorders (PFDI-20 and PFIQ-7). *American Journal of
  Obstetrics and Gynecology, 193*(1), 103-113.

- **Yalcin I, Bump RC** (2003). Validation of two global impression
  questionnaires for incontinence. *American Journal of Obstetrics and
  Gynecology, 189*(1), 98-101.

- **Sandvik H, Seim A, Vanvik A, Hunskaar S** (2000). A severity index
  for epidemiological surveys of female urinary incontinence: Comparison
  with 48-hour pad-weighing tests. *Neurourology and Urodynamics,
  19*(2), 137-145.

- **Jelovsek JE, Barber MD** (2006). Patient-reported outcome measures
  in pelvic floor disorders. *American Journal of Obstetrics and
  Gynecology, 194*(5), 1455-1461.

## Contributing

Contributions to urogynScorer are welcome! Please submit issues and pull
requests on GitHub.

## License

MIT + file LICENSE

## Support

If you need help with using the urogynScorer package or have questions,
there are several ways to get support:

- **GitHub Issues**: For bug reports, feature requests, or general
  questions, please [open an
  issue](https://github.com/mufflyt/urogynScorer/issues) on the GitHub
  repository.
- **Email**: For direct assistance, you can email the package maintainer
  at <tyler.muffly@dhha.org>.
- **Office Hours**: For Denver Health clinicians, in-person
  consultations are available on request.

## Roadmap

Future development plans for the urogynScorer package include:

- **Version 0.3.0** (Q3 2025):
  - Add support for PFIQ-7 (Pelvic Floor Impact Questionnaire)
  - Implement PISQ-12 (Pelvic Organ Prolapse/Urinary Incontinence Sexual
    Questionnaire)
  - Improve visualization tools for longitudinal patient data
- **Version 0.4.0** (Q1 2026):
  - Add support for OAB-q SF (Overactive Bladder Questionnaire Short
    Form)
  - Implement ICIQ-UI SF (International Consultation on Incontinence
    Questionnaire-Urinary Incontinence Short Form)
  - Create interactive Shiny dashboard for clinical use
- **Version 1.0.0** (Q3 2026):
  - Complete implementation of all major urogynecology PROMs
  - Full internationalization support
  - Integration with REDCap and other clinical databases

## Project Status

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

This package is in active development with regular updates. The current
stable version (0.2.0) implements the core questionnaires most commonly
used in urogynecology practice and research. Development is proceeding
according to the roadmap above, with a focus on adding support for
additional validated instruments and improving usability for clinicians
and researchers.

## Code of Conduct

Please note that the urogynScorer project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms. \`\`\`
— © 2025 Tyler Muffly, MD and Denver Health and Hospital Authority
