---
title: "CONTRIBUTING"
output: html_document
---

# Contributing to urogynScorer

Thank you for your interest in contributing to urogynScorer! This package aims to provide standardized tools for scoring urogynecology questionnaires and assessments. We welcome contributions from the community.

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## How to Contribute

### Reporting Issues

If you encounter a bug or have a feature request, please submit an issue on our [GitHub issue tracker](https://github.com/mufflyt/urogynScorer/issues). When reporting a bug, please include:

- A clear description of the issue
- Steps to reproduce the problem
- Expected behavior
- Actual behavior
- R session information (`sessionInfo()`)
- Example data if possible (without PHI/PII)

### Pull Requests

We welcome pull requests for bug fixes, features, documentation improvements, or other enhancements. Here's how to submit a pull request:

1. Fork the repository
2. Create a new branch for your feature (`git checkout -b feature/my-new-feature`)
3. Make your changes
4. Run tests to ensure they pass (`devtools::test()`)
5. Update documentation if needed (`devtools::document()`)
6. Commit your changes (`git commit -am 'Add some feature'`)
7. Push to the branch (`git push origin feature/my-new-feature`)
8. Create a new Pull Request

### Coding Style

Please follow these coding standards when contributing:

- Use the [tidyverse style guide](https://style.tidyverse.org/) for R code
- Document all functions with roxygen2 comments
- Include examples in function documentation
- Write tests for new functions or bug fixes
- Use meaningful variable names that reflect the clinical context

### Adding New Questionnaires

If you're adding support for a new questionnaire or assessment tool:

1. Ensure the scoring algorithm is based on published validation studies
2. Include proper references to the original validation paper(s)
3. Implement robust validation of input data
4. Add appropriate handling of missing values
5. Write comprehensive tests for various input scenarios
6. Create a vignette demonstrating usage of the new scoring function

## Development Workflow

1. Install development dependencies:
   ```r
   install.packages(c("devtools", "testthat", "roxygen2", "knitr", "rmarkdown"))
   ```

2. Clone the repository and install the package in development mode:
   ```r
   devtools::install_github("mufflyt/urogynScorer", dependencies = TRUE)
   ```

3. Run tests to ensure everything is working:
   ```r
   devtools::test()
   ```

4. Build and check the package:
   ```r
   devtools::check()
   ```

Thank you for contributing to urogynScorer!
