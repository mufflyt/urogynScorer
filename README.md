urogynScorer: Urogynecology Questionnaire Scoring Package
================
Tyler Muffly, MD
2025-03-28

<!-- README.md is generated from README.Rmd. Please edit that file -->

<a href="https://mufflyt.github.io/urogynScorer/"><img src="man/figures/logo.png" align="right" height="240" alt="urogynScorer website" /></a>

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
- **Pelvic Floor Impact Questionnaire (PFIQ-7)**
- **Pelvic Organ Prolapse/Urinary Incontinence Sexual Questionnaire
  (PISQ-12)**
- **Overactive Bladder Questionnaire Short Form (OAB-q SF)**
- **International Consultation on Incontinence Questionnaire-Urinary
  Incontinence Short Form (ICIQ-UI SF)**
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

### Pelvic Organ Prolapse/Urinary Incontinence Sexual Questionnaire (PISQ-12)

The Pelvic Organ Prolapse/Urinary Incontinence Sexual Questionnaire
(PISQ-12) is a condition-specific tool that evaluates sexual function in
women with pelvic organ prolapse and/or urinary incontinence:

``` r
# Create sample data
pisq_data <- data.frame(
  Record_ID = c("PT001", "PT002", "PT003"),
  pisq_q1 = c(2, 3, 1),    # Questions 1-4 are scored 0-4 (higher = better)
  pisq_q2 = c(3, 2, 0),
  pisq_q3 = c(1, 2, 4),
  pisq_q4 = c(2, 1, 3),
  pisq_q5 = c(2, 1, 0),    # Questions 5-12 are scored 0-4 (lower = better)
  pisq_q6 = c(1, 2, 3),
  pisq_q7 = c(0, 1, 2),
  pisq_q8 = c(1, 0, 2),
  pisq_q9 = c(2, 1, 0),
  pisq_q10 = c(3, 2, 1),
  pisq_q11 = c(1, 2, 0),
  pisq_q12 = c(0, 1, 3)
)

# Score PISQ-12
pisq_results <- calculate_pisq12(
  questionnaire_data = pisq_data,
  id_column = "Record_ID",
  missing_threshold = 0.2,  # Allow up to 20% missing data
  verbose = TRUE            # Enable detailed logging
)

# View results
pisq_results
```

### International Consultation on Incontinence Questionnaire-Urinary Incontinence Short Form (ICIQ-UI SF)

The ICIQ-UI SF measures urinary incontinence symptoms and their impact
on quality of life:

``` r
# Create sample data
iciq_data <- data.frame(
  patient_id = c("P001", "P002", "P003"),
  iciq_1 = c(2, 3, 1),     # Frequency (0-5)
  iciq_2 = c(4, 2, 0),     # Volume (0-6)
  iciq_3 = c(6, 8, 3),     # Impact (0-10)
  iciq_4_1 = c(1, 0, 0),   # Situation: leaks before reaching toilet
  iciq_4_2 = c(0, 1, 0),   # Situation: leaks when coughing/sneezing
  iciq_4_3 = c(0, 0, 1),   # Situation: leaks when asleep
  iciq_4_4 = c(0, 1, 0),   # Situation: leaks with physical activity
  iciq_4_5 = c(1, 0, 0),   # Situation: leaks when finished urinating
  iciq_4_6 = c(0, 0, 0),   # Situation: leaks for no obvious reason
  iciq_4_7 = c(0, 0, 0)    # Situation: leaks all the time
)

# Score ICIQ-UI SF
iciq_results <- calculate_iciq_ui_sf(
  questionnaire_data = iciq_data,
  id_column = "patient_id",
  missing_threshold = 0.0,  # No missing values allowed by default
  verbose = TRUE            # Enable detailed logging
)

# View results
iciq_results
```

### Overactive Bladder Questionnaire Short Form (OAB-q SF)

The OAB-q SF evaluates symptom bother and health-related quality of life
in patients with overactive bladder:

``` r
# Create sample data
oab_data <- data.frame(
  patient_id = c("P001", "P002", "P003"),
  oab_1 = c(2, 4, 3),      # Symptom items (1-6)
  oab_2 = c(3, 5, 2),
  oab_3 = c(4, 3, 1),
  oab_4 = c(2, 4, 3),
  oab_5 = c(3, 2, 4),
  oab_6 = c(1, 3, 2),
  oab_7 = c(4, 2, 5),      # HRQL items (7-19)
  oab_8 = c(5, 3, 4),
  oab_9 = c(3, 4, 5),
  oab_10 = c(4, 3, 4),
  oab_11 = c(5, 2, 3),
  oab_12 = c(4, 3, 5),
  oab_13 = c(3, 4, 4),
  oab_14 = c(5, 3, 3),
  oab_15 = c(4, 5, 4),
  oab_16 = c(3, 4, 5),
  oab_17 = c(4, 3, 4),
  oab_18 = c(5, 4, 3),
  oab_19 = c(4, 5, 4)
)

# Score OAB-q SF
oab_results <- calculate_oabq_sf(
  questionnaire_data = oab_data,
  id_column = "patient_id",
  symptom_missing_threshold = 0.5,  # Allow up to 50% missing data for symptom scale
  hrql_missing_threshold = 0.5,     # Allow up to 50% missing data for HRQL scale
  verbose = TRUE                    # Enable detailed logging
)

# View results
oab_results
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

## Complete Questionnaire Items

### Pelvic Floor Distress Inventory (PFDI-20) Items

The PFDI-20 consists of 20 questions across three subscales:

#### POPDI-6 (Questions 1-6)

1.  Do you usually experience pressure in the lower abdomen?
2.  Do you usually experience heaviness or dullness in the pelvic area?
3.  Do you usually have a bulge or something falling out that you can
    see or feel in the vaginal area?
4.  Do you usually have to push on the vagina or around the rectum to
    have or complete a bowel movement?
5.  Do you usually experience a feeling of incomplete bladder emptying?
6.  Do you ever have to push up on a bulge in the vaginal area with your
    fingers to start or complete urination?

#### CRADI-8 (Questions 7-14)

7.  Do you feel you need to strain too hard to have a bowel movement?
8.  Do you feel you have not completely emptied your bowels at the end
    of a bowel movement?
9.  Do you usually lose stool beyond your control if your stool is well
    formed?
10. Do you usually lose stool beyond your control if your stool is loose
    or liquid?
11. Do you usually lose gas from the rectum beyond your control?
12. Do you usually have pain when you pass your stool?
13. Do you experience a strong sense of urgency and have to rush to the
    bathroom to have a bowel movement?
14. Does a part of your bowel ever pass through the rectum and bulge
    outside during or after a bowel movement?

#### UDI-6 (Questions 15-20)

15. Do you usually experience frequent urination?
16. Do you usually experience urine leakage associated with a feeling of
    urgency; that is, a strong sensation of needing to go to the
    bathroom?
17. Do you usually experience urine leakage related to coughing,
    sneezing, or laughing?
18. Do you usually experience small amounts of urine leakage (drops)?
19. Do you usually experience difficulty emptying your bladder?
20. Do you usually experience pain or discomfort in the lower abdomen or
    genital region?

Each item is scored on a scale of 0-4: - 0: Not at all - 1: Somewhat -
2: Moderately - 3: Quite a bit - 4: Extremely

### Pelvic Floor Impact Questionnaire (PFIQ-7) Items

The PFIQ-7 asks how bladder, bowel, or vaginal symptoms affect
activities, relationships, and feelings across three scales:

#### Urinary Impact Questionnaire (UIQ-7)

1.  Ability to do household chores (cooking, housecleaning, laundry)?
2.  Ability to do physical activities such as walking, swimming, or
    other exercise?
3.  Entertainment activities (going to a movie, concert, etc.)?
4.  Ability to travel by car or bus for a distance greater than 30
    minutes away from home?
5.  Participating in social activities outside your home?
6.  Emotional health (nervousness, depression, etc.)?
7.  Feeling frustrated?

#### Colorectal-Anal Impact Questionnaire (CRAIQ-7)

1.  Ability to do household chores (cooking, housecleaning, laundry)?
2.  Ability to do physical activities such as walking, swimming, or
    other exercise?
3.  Entertainment activities (going to a movie, concert, etc.)?
4.  Ability to travel by car or bus for a distance greater than 30
    minutes away from home?
5.  Participating in social activities outside your home?
6.  Emotional health (nervousness, depression, etc.)?
7.  Feeling frustrated?

#### Pelvic Organ Prolapse Impact Questionnaire (POPIQ-7)

1.  Ability to do household chores (cooking, housecleaning, laundry)?
2.  Ability to do physical activities such as walking, swimming, or
    other exercise?
3.  Entertainment activities (going to a movie, concert, etc.)?
4.  Ability to travel by car or bus for a distance greater than 30
    minutes away from home?
5.  Participating in social activities outside your home?
6.  Emotional health (nervousness, depression, etc.)?
7.  Feeling frustrated?

Each item is scored on a scale of 0-3: - 0: Not at all - 1: Somewhat -
2: Moderately - 3: Quite a bit

### Pelvic Organ Prolapse/Urinary Incontinence Sexual Questionnaire (PISQ-12) Items

The PISQ-12 assesses sexual function in women with pelvic floor
disorders:

1.  How frequently do you feel sexual desire? This feeling may include
    wanting to have sex, planning to have sex, feeling frustrated due to
    lack of sex, etc.
2.  Do you climax (have an orgasm) when having sexual intercourse with
    your partner?
3.  Do you feel sexually excited (turned on) when having sexual activity
    with your partner?
4.  How satisfied are you with the variety of sexual activities in your
    current sex life?
5.  Do you feel pain during sexual intercourse?
6.  Are you incontinent of urine (leak urine) with sexual activity?
7.  Does fear of incontinence (either stool or urine) restrict your
    sexual activity?
8.  Do you avoid sexual intercourse because of bulging in the vagina
    (either the bladder, rectum, or vagina falling out)?
9.  When you have sex with your partner, do you have negative emotional
    reactions such as fear, disgust, shame or guilt?
10. Does your partner have a problem with erections that affects your
    sexual activity?
11. Does your partner have a problem with premature ejaculation that
    affects your sexual activity?
12. Compared to orgasms you have had in the past, how intense are the
    orgasms you have had in the past six months?

Each item is scored on a scale of 0-4: - 0: Always - 1: Usually - 2:
Sometimes - 3: Seldom - 4: Never

For questions 1-4, the scoring is reversed (0=4, 1=3, etc.) since they
are positively phrased.

### Overactive Bladder Questionnaire Short Form (OAB-q SF) Items

The OAB-q SF has two sections: Symptom Bother (questions 1-6) and
Health-Related Quality of Life (questions 7-19).

#### Symptom Bother (Questions 1-6)

During the past 4 weeks, how bothered were you by: 1. An uncomfortable
urge to urinate? 2. A sudden urge to urinate with little or no warning?
3. Accidental loss of small amounts of urine? 4. Nighttime urination? 5.
Waking up at night because you had to urinate? 6. An uncontrollable urge
to urinate?

#### Health-Related Quality of Life (Questions 7-19)

During the past 4 weeks, how often have your bladder symptoms: 7. Caused
you to plan escape routes to restrooms in public places? 8. Made you
feel like there is something wrong with you? 9. Made you feel depressed?
10. Made you feel frustrated? 11. Made you feel worried about bothering
others because of your frequent trips to the bathroom? 12. Made you feel
worried about being far away from a restroom? 13. Affected your
relationship with family and friends? 14. Caused you to stay home more
than you would prefer? 15. Caused you to have problems with a good
night’s sleep? 16. Caused you to have problems with shopping, working
outside the home, or traveling? 17. Caused you embarrassment? 18. Caused
you to locate the nearest restroom as soon as you arrive at a place
you’ve never been? 19. Caused you to avoid activities away from
restrooms (i.e., walks, running, hiking)?

Each item is scored on a scale of 1-6: - 1: Not at all - 2: A little
bit - 3: Somewhat - 4: Quite a bit - 5: A great deal - 6: A very great
deal

### International Consultation on Incontinence Questionnaire-Urinary Incontinence Short Form (ICIQ-UI SF) Items

The ICIQ-UI SF consists of 4 items:

1.  How often do you leak urine? (0-5)

    - 0: Never
    - 1: About once a week or less often
    - 2: Two or three times a week
    - 3: About once a day
    - 4: Several times a day
    - 5: All the time

2.  How much urine do you usually leak (whether you wear protection or
    not)? (0-6)

    - 0: None
    - 2: A small amount
    - 4: A moderate amount
    - 6: A large amount

3.  Overall, how much does leaking urine interfere with your everyday
    life? (0-10)

    - 0: Not at all
    - 10: A great deal

    (Score is marked on a visual analog scale from 0-10)

4.  When does urine leak? (check all that apply) - Not scored,
    diagnostic only

    - When you cough or sneeze
    - When you are physically active/exercising
    - Before you can get to the toilet
    - When you are asleep
    - When you have finished urinating and are dressed
    - For no obvious reason
    - All the time

### Patient Global Impression of Improvement (PGI-I) Item

The PGI-I consists of a single question:

“Compared to how you were before your treatment, how would you describe
your condition now?”

- 1: Very much better
- 2: Much better
- 3: A little better
- 4: No change
- 5: A little worse
- 6: Much worse
- 7: Very much worse

### Sandvik Severity Index Items

The Sandvik Severity Index consists of two questions:

1.  How often do you experience urine leakage? (Frequency)
    - 0: Never
    - 1: Less than once a month
    - 2: A few times a month
    - 3: A few times a week
    - 4: Every day and/or night
2.  How much urine do you lose each time? (Amount)
    - 1: Drops
    - 2: Small amounts
    - 3: Large amounts

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

### ICIQ-UI SF Scoring

The ICIQ-UI SF consists of 4 items:

| Item  | Description                    | Score Range             |
|-------|--------------------------------|-------------------------|
| **1** | Frequency of urinary leakage   | 0-5                     |
| **2** | Amount of leakage              | 0-6                     |
| **3** | Impact on quality of life      | 0-10                    |
| **4** | Situations when leakage occurs | Not scored (diagnostic) |

**Scoring process:** 1. Sum items 1-3 to get the total score (range
0-21) 2. Higher scores indicate greater severity: - 1-5: Mild urinary
incontinence - 6-12: Moderate urinary incontinence - 13-21: Severe
urinary incontinence 3. Item 4 (situations) provides diagnostic
information about the type of incontinence

### PFIQ-7 Scoring

The PFIQ-7 consists of three scales with 7 questions each:

| Scale       | Description                                |
|-------------|--------------------------------------------|
| **UIQ-7**   | Urinary Impact Questionnaire               |
| **CRAIQ-7** | Colorectal-Anal Impact Questionnaire       |
| **POPIQ-7** | Pelvic Organ Prolapse Impact Questionnaire |

Each question is scored from 0-3: - 0: Not at all - 1: Somewhat - 2:
Moderately - 3: Quite a bit

**Scoring process:** 1. Calculate the sum for each scale 2. Multiply by
(100/21) to get a 0-100 scale 3. Sum the three scales to get the total
PFIQ-7 score (0-300 scale)

### PISQ-12 Scoring

The Pelvic Organ Prolapse/Urinary Incontinence Sexual Questionnaire
(PISQ-12) evaluates sexual function in women with pelvic floor
disorders:

| Questions | Direction                                      |
|-----------|------------------------------------------------|
| **1-4**   | Higher score = better function (reverse-coded) |
| **5-12**  | Lower score = better function                  |

Each question is scored from 0-4: - 0: Always - 1: Usually - 2:
Sometimes - 3: Seldom - 4: Never

**Scoring process:** 1. Reverse-code questions 1-4 (0=4, 1=3, etc.) 2.
Sum all items (range 0-48) 3. Transform to a 0-100 scale: (score/48) ×
100 4. Higher scores indicate better sexual function

### OAB-q SF Scoring

The OAB-q SF consists of two scales:

| Scale | Questions | Description |
|----|----|----|
| **Symptom Bother** | 1-6 | Measures degree of bother from overactive bladder symptoms |
| **Health-Related Quality of Life (HRQL)** | 7-19 | Measures impact on quality of life |

Each question is scored from 1-6: - 1: Not at all - 2: A little bit - 3:
Somewhat - 4: Quite a bit - 5: A great deal - 6: A very great deal

**Scoring process:** 1. Calculate raw scores for each scale (sum of
items) 2. Transform to a 0-100 scale using formula: ((mean-1)/5) × 100
3. For Symptom Bother scale, higher scores indicate greater symptom
bother 4. For HRQL scale, higher scores indicate better quality of life

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

### ICIQ-UI SF Scoring Algorithm

The ICIQ-UI SF consists of 3 scored items plus a diagnostic item:

1.  The total score is the sum of items 1-3:

![\text{ICIQ-UI SF Score} = \text{Frequency Score} + \text{Volume Score} + \text{Impact Score}](https://latex.codecogs.com/png.latex?%5Ctext%7BICIQ-UI%20SF%20Score%7D%20%3D%20%5Ctext%7BFrequency%20Score%7D%20%2B%20%5Ctext%7BVolume%20Score%7D%20%2B%20%5Ctext%7BImpact%20Score%7D "\text{ICIQ-UI SF Score} = \text{Frequency Score} + \text{Volume Score} + \text{Impact Score}")

where: - Frequency Score ranges from 0-5 - Volume Score ranges from
0-6 - Impact Score ranges from 0-10

The resulting ICIQ-UI SF score ranges from 0-21, with severity
categories: - 1-5: Mild urinary incontinence - 6-12: Moderate urinary
incontinence - 13-21: Severe urinary incontinence

Missing data is handled according to the missing threshold parameter,
with imputation based on the mean of non-missing items when permissible.

### PFIQ-7 Scoring

For each of the three scales (UIQ-7, CRAIQ-7, POPIQ-7), with 7 questions
each scored 0-3:

1.  Calculate the sum of all valid responses for each scale:

    ![\text{Scale Sum} = \sum\_{i=1}^{7} q_i](https://latex.codecogs.com/png.latex?%5Ctext%7BScale%20Sum%7D%20%3D%20%5Csum_%7Bi%3D1%7D%5E%7B7%7D%20q_i "\text{Scale Sum} = \sum_{i=1}^{7} q_i")

2.  Scale to a 0-100 range:

    ![\text{Scale Score} = \text{Scale Sum} \times \frac{100}{21}](https://latex.codecogs.com/png.latex?%5Ctext%7BScale%20Score%7D%20%3D%20%5Ctext%7BScale%20Sum%7D%20%5Ctimes%20%5Cfrac%7B100%7D%7B21%7D "\text{Scale Score} = \text{Scale Sum} \times \frac{100}{21}")

3.  Calculate the total PFIQ-7 score:

    ![\text{PFIQ-7 Total} = \text{UIQ-7} + \text{CRAIQ-7} + \text{POPIQ-7}](https://latex.codecogs.com/png.latex?%5Ctext%7BPFIQ-7%20Total%7D%20%3D%20%5Ctext%7BUIQ-7%7D%20%2B%20%5Ctext%7BCRAIQ-7%7D%20%2B%20%5Ctext%7BPOPIQ-7%7D "\text{PFIQ-7 Total} = \text{UIQ-7} + \text{CRAIQ-7} + \text{POPIQ-7}")

### Pelvic Organ Prolapse/Urinary Incontinence Sexual Questionnaire (PISQ-12) Scoring

The PISQ-12 has 12 questions, with questions 1-4 reverse-coded:

1.  After reverse-coding questions 1-4, sum all responses:

    ![\text{PISQ-12 Raw} = \sum\_{i=1}^{12} q_i](https://latex.codecogs.com/png.latex?%5Ctext%7BPISQ-12%20Raw%7D%20%3D%20%5Csum_%7Bi%3D1%7D%5E%7B12%7D%20q_i "\text{PISQ-12 Raw} = \sum_{i=1}^{12} q_i")

2.  Scale to a 0-100 range:

    ![\text{PISQ-12 Score} = \frac{\text{PISQ-12 Raw}}{48} \times 100](https://latex.codecogs.com/png.latex?%5Ctext%7BPISQ-12%20Score%7D%20%3D%20%5Cfrac%7B%5Ctext%7BPISQ-12%20Raw%7D%7D%7B48%7D%20%5Ctimes%20100 "\text{PISQ-12 Score} = \frac{\text{PISQ-12 Raw}}{48} \times 100")

Higher scores indicate better sexual function.

### Overactive Bladder Questionnaire Short Form (OAB-q SF) Scoring

The OAB-q SF consists of two separate scales:

1.  For the Symptom Bother scale (6 items), calculate the transformed
    score:

![\text{Symptom Score} = \frac{(\bar{q}\_{\text{symptom}} - 1)}{5} \times 100](https://latex.codecogs.com/png.latex?%5Ctext%7BSymptom%20Score%7D%20%3D%20%5Cfrac%7B%28%5Cbar%7Bq%7D_%7B%5Ctext%7Bsymptom%7D%7D%20-%201%29%7D%7B5%7D%20%5Ctimes%20100 "\text{Symptom Score} = \frac{(\bar{q}_{\text{symptom}} - 1)}{5} \times 100")

where
![\bar{q}\_{\text{symptom}}](https://latex.codecogs.com/png.latex?%5Cbar%7Bq%7D_%7B%5Ctext%7Bsymptom%7D%7D "\bar{q}_{\text{symptom}}")
is the mean of the 6 symptom items.

2.  For the Health-Related Quality of Life scale (13 items), calculate:

![\text{HRQL Score} = \frac{(\bar{q}\_{\text{hrql}} - 1)}{5} \times 100](https://latex.codecogs.com/png.latex?%5Ctext%7BHRQL%20Score%7D%20%3D%20%5Cfrac%7B%28%5Cbar%7Bq%7D_%7B%5Ctext%7Bhrql%7D%7D%20-%201%29%7D%7B5%7D%20%5Ctimes%20100 "\text{HRQL Score} = \frac{(\bar{q}_{\text{hrql}} - 1)}{5} \times 100")

where
![\bar{q}\_{\text{hrql}}](https://latex.codecogs.com/png.latex?%5Cbar%7Bq%7D_%7B%5Ctext%7Bhrql%7D%7D "\bar{q}_{\text{hrql}}")
is the mean of the 13 HRQL items.

For the Symptom scale, higher scores indicate greater symptom bother.
For the HRQL scale, higher scores indicate better quality of life

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

# View PISQ-12 vignette
vignette("Scoring_the_Pelvic_Organ_Prolapse_Urinary_Incontinence_Sexual_Questionnaire", package = "urogynScorer")

# View OAB-q SF vignette
vignette("Scoring_the_Overactive_Bladder_Questionnaire_Short_Form", package = "urogynScorer")

# View ICIQ-UI SF vignette
vignette("International_Consultation_on_Incontinence_Questionnaire", package = "urogynScorer")
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

- **Rogers RG, Coates KW, Kammerer-Doak D, Khalsa S, Qualls C** (2003).
  A short form of the Pelvic Organ Prolapse/Urinary Incontinence Sexual
  Questionnaire (PISQ-12). *International Urogynecology Journal and
  Pelvic Floor Dysfunction, 14*(3), 164-168.

- **Coyne KS, Thompson CL, Lai JS, Sexton CC** (2015). An overactive
  bladder symptom and health-related quality of life short-form:
  validation of the OAB-q SF. *Neurourology and Urodynamics, 34*(3),
  255-263.

- **Avery K, Donovan J, Peters TJ, Shaw C, Gotoh M, Abrams P** (2004).
  ICIQ: a brief and robust measure for evaluating the symptoms and
  impact of urinary incontinence. *Neurourology and Urodynamics, 23*(4),
  322-330.

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
  - Implement Pelvic Organ Prolapse/Urinary Incontinence Sexual
    Questionnaire (PISQ-12)
  - Implement Overactive Bladder Questionnaire Short Form (OAB-q SF)
  - Implement International Consultation on Incontinence
    Questionnaire-Urinary Incontinence Short Form (ICIQ-UI SF)
  - Improve visualization tools for longitudinal patient data
- **Version 0.4.0** (Q1 2026):
  - Add support for EPAQ (Electronic Personal Assessment Questionnaire)
  - Create interactive Shiny dashboard for clinical use
  - Add support for longitudinal data analysis
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
