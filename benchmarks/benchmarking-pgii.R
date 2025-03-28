#' Benchmark the score_pgii function
#'
#' This script benchmarks the performance of the score_pgii function
#' with different dataset sizes and configurations.
#'
#' @note This script is meant to be run during development and is not part of the package itself.
#' @noRd

# Load necessary libraries
library(bench)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(dplyr)

# Generate test datasets of different sizes
create_test_dataset <- function(n_rows) {
  set.seed(123) # For reproducibility
  data.frame(
    Record_ID = paste0("P", sprintf("%05d", 1:n_rows)),
    pgii_response = sample(1:7, n_rows, replace = TRUE)
  )
}

# Create datasets with varying numbers of rows
dataset_sizes <- c(10, 100, 1000, 10000, 50000)
test_datasets <- map(dataset_sizes, create_test_dataset)
names(test_datasets) <- paste0("n", dataset_sizes)

# Benchmark score_pgii with different dataset sizes
size_benchmarks <- bench::press(
  size = dataset_sizes,
  {
    df <- test_datasets[[paste0("n", size)]]
    bench::mark(
      score_pgii(df, "pgii_response", verbose = FALSE),
      iterations = 10,
      check = FALSE
    )
  }
)

# Create a dataset with 1000 rows and varying levels of missing data
create_missing_data_dataset <- function(n_rows, missing_pct) {
  set.seed(123) # For reproducibility
  df <- data.frame(
    Record_ID = paste0("P", sprintf("%05d", 1:n_rows)),
    pgii_response = sample(1:7, n_rows, replace = TRUE)
  )

  # Randomly set some values to NA
  missing_indices <- sample(1:n_rows, size = floor(n_rows * missing_pct))
  df$pgii_response[missing_indices] <- NA

  return(df)
}

# Create datasets with varying levels of missing data
missing_pcts <- c(0, 0.1, 0.25, 0.5, 0.75, 0.9)
missing_datasets <- map(missing_pcts, ~create_missing_data_dataset(1000, .x))
names(missing_datasets) <- paste0("miss", missing_pcts * 100)

# Benchmark score_pgii with different levels of missing data
missing_benchmarks <- bench::press(
  missing_pct = missing_pcts,
  {
    df <- missing_datasets[[paste0("miss", missing_pct * 100)]]
    bench::mark(
      score_pgii(df, "pgii_response", verbose = FALSE),
      iterations = 10,
      check = FALSE
    )
  }
)

# Benchmark the effect of verbose logging
verbose_benchmarks <- bench::press(
  verbose = c(FALSE, TRUE),
  size = c(100, 1000),
  {
    df <- test_datasets[[paste0("n", size)]]
    bench::mark(
      score_pgii(df, "pgii_response", verbose = verbose),
      iterations = 10,
      check = FALSE
    )
  }
)

# Benchmark the effect of different options
options_benchmarks <- bench::press(
  size = 1000,
  keep_n_valid = c(FALSE, TRUE),
  output_file = c(NA, tempfile()),
  {
    df <- test_datasets[["n1000"]]
    if (is.na(output_file)) {
      bench::mark(
        score_pgii(df, "pgii_response", keep_n_valid = keep_n_valid, verbose = FALSE),
        iterations = 10,
        check = FALSE
      )
    } else {
      bench::mark(
        score_pgii(df, "pgii_response", keep_n_valid = keep_n_valid,
                   output_file = output_file, verbose = FALSE),
        iterations = 10,
        check = FALSE
      )
    }
  }
)

# Plot the results
plot_size_benchmarks <- size_benchmarks %>%
  mutate(size = paste0("n = ", size)) %>%
  ggplot(aes(x = size, y = median)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = min, ymax = max), width = 0.2) +
  labs(
    title = "score_pgii() Performance by Dataset Size",
    subtitle = "Median execution time with min/max range",
    x = "Dataset Size",
    y = "Execution Time (seconds)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_missing_benchmarks <- missing_benchmarks %>%
  mutate(missing = paste0(missing_pct * 100, "% Missing")) %>%
  ggplot(aes(x = missing, y = median)) +
  geom_col(fill = "coral") +
  geom_errorbar(aes(ymin = min, ymax = max), width = 0.2) +
  labs(
    title = "score_pgii() Performance with Missing Data",
    subtitle = "Dataset size = 1000 rows",
    x = "Percentage of Missing Values",
    y = "Execution Time (seconds)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_verbose_benchmarks <- verbose_benchmarks %>%
  mutate(
    verbose = ifelse(verbose, "Verbose = TRUE", "Verbose = FALSE"),
    size = paste0("n = ", size)
  ) %>%
  ggplot(aes(x = verbose, y = median, fill = size)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = min, ymax = max, group = size),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  labs(
    title = "Effect of Verbose Logging on Performance",
    x = "Verbose Setting",
    y = "Execution Time (seconds)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Print summary of results
cat("\n=== Performance by Dataset Size ===\n")
print(size_benchmarks %>% dplyr::select(size, min, median, `max`, `itr/sec`))

cat("\n=== Performance with Missing Data ===\n")
print(missing_benchmarks %>% select(missing_pct, min, median, max, n_itr))

cat("\n=== Performance with Different Verbose Settings ===\n")
print(verbose_benchmarks %>% select(verbose, size, min, median, max, n_itr))

cat("\n=== Performance with Different Options ===\n")
print(options_benchmarks %>% select(keep_n_valid, output_file, min, median, max, n_itr))

# Save plots if desired
# ggsave("benchmark_size.png", plot_size_benchmarks, width = 8, height = 6)
# ggsave("benchmark_missing.png", plot_missing_benchmarks, width = 8, height = 6)
# ggsave("benchmark_verbose.png", plot_verbose_benchmarks, width = 8, height = 6)

# Performance report summary
cat("\n\n=== PERFORMANCE REPORT SUMMARY ===\n")
cat("The score_pgii() function was benchmarked under various conditions to assess its performance characteristics.\n\n")

# Calculate scaling factor from 100 to 1000 rows
scaling_factor_100_1000 <- size_benchmarks %>%
  filter(size %in% c(100, 1000)) %>%
  arrange(size) %>%
  pull(median) %>%
  {.[2] / .[1]}

cat(sprintf("Scaling: The function scales approximately %.2fx when dataset size increases 10x (100 to 1000 rows).\n",
            scaling_factor_100_1000))

# Calculate overhead of verbose mode
verbose_overhead <- verbose_benchmarks %>%
  group_by(size) %>%
  summarize(overhead = median[verbose] / median[!verbose]) %>%
  pull(overhead) %>%
  mean()

cat(sprintf("Verbose Mode: Enabling verbose logging adds approximately %.2fx overhead to execution time.\n",
            verbose_overhead))

# Calculate impact of missing values
missing_impact <- missing_benchmarks %>%
  arrange(missing_pct) %>%
  mutate(rel_performance = median / median[1]) %>%
  filter(missing_pct == max(missing_pct)) %>%
  pull(rel_performance)

cat(sprintf("Missing Values: A dataset with %.0f%% missing values runs approximately %.2fx %s than a dataset with no missing values.\n",
            max(missing_pcts) * 100,
            abs(missing_impact),
            ifelse(missing_impact < 1, "faster", "slower")))

# Calculate overhead of writing to file
file_overhead <- options_benchmarks %>%
  filter(!is.na(output_file), !keep_n_valid) %>%
  pull(median) /
  options_benchmarks %>%
  filter(is.na(output_file), !keep_n_valid) %>%
  pull(median)

cat(sprintf("File Output: Writing results to a file adds approximately %.2fx overhead to execution time.\n",
            file_overhead))

# Calculate overhead of keep_n_valid
keep_n_valid_overhead <- options_benchmarks %>%
  filter(is.na(output_file), keep_n_valid) %>%
  pull(median) /
  options_benchmarks %>%
  filter(is.na(output_file), !keep_n_valid) %>%
  pull(median)

cat(sprintf("keep_n_valid: Enabling the keep_n_valid option adds approximately %.2fx overhead to execution time.\n",
            keep_n_valid_overhead))
