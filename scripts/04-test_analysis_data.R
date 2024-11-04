#### Preamble ####
# Purpose: This code defines test cases for validating the integrity, range, and data types of key columns in the processed `candidate_sup_num_data` and `num_grade_pollscore_data` datasets, ensuring they meet expected standards for data quality and consistency.
# Author: Yingke He, Ziheng Zhong
# Date: 20 October 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT



#### Workspace setup ####
library(tidyverse)
library(testthat)
library(here)



#### Test cases ####
# candidate_sup_num_data Tests
test_that("Data Integrity Test: No missing values in critical columns", {
  candidate_data <- read_csv(here("data", "02-analysis_data", "candidate_sup_num_data.csv"))
  print(names(candidate_data))  # Debugging: Print column names to verify
  critical_columns <- c("candidate", "support_pct")  # Allow missing values in weight_value
  for (col in critical_columns) {
    expect_false(any(is.na(candidate_data[[col]])), paste("Column", col, "contains missing values."))
  }
})


test_that("Range Validation Test: Numerical columns are within expected ranges", {
  candidate_data <- read_csv(here("data", "02-analysis_data", "candidate_sup_num_data.csv"))
  print(names(candidate_data))  # Debugging: Print column names to verify
  # Assuming weight_value should be between 0 and 10000, ignoring NA values
  expect_true(all(candidate_data$weight_value[!is.na(candidate_data$weight_value)] >= 0 &
                    candidate_data$weight_value[!is.na(candidate_data$weight_value)] <= 10000),
              "Weight value is out of the expected range (0-10000).")
})


test_that("Column Type Test: All columns have expected data types", {
  candidate_data <- read_csv(here("data", "02-analysis_data", "candidate_sup_num_data.csv"))
  print(names(candidate_data))  # Debugging: Print column names to verify
  expect_true(is.character(candidate_data$candidate), "candidate is not a character")
  expect_true(is.numeric(candidate_data$weight_value), "weight_value is not numeric")
  expect_true(is.character(candidate_data$date), "date is not a character")
})



# num_grade_pollscore_data Tests
test_that("Data Integrity Test: No missing values in critical columns", {
  pollscore_data <- read_csv(here("data", "02-analysis_data", "num_grade_pollscore_data.csv"))
  print(names(pollscore_data))  # Debugging: Print column names to verify
  critical_columns <- c("numeric_grade", "pollscore")
  for (col in critical_columns) {
    expect_false(any(is.na(pollscore_data[[col]])), paste("Column", col, "contains missing values."))
  }
})


test_that("Range Validation Test: Numerical columns are within expected ranges", {
  pollscore_data <- read_csv(here("data", "02-analysis_data", "num_grade_pollscore_data.csv"))
  print(names(pollscore_data))  # Debugging: Print column names to verify
  # Assuming pollscore should be between 0 and 100, ignoring NA values
  out_of_range <- pollscore_data$pollscore[!is.na(pollscore_data$pollscore) & (pollscore_data$pollscore < 0 | pollscore_data$pollscore > 100)]
  if (length(out_of_range) > 0) {
    print(paste("Out of range values in pollscore:", paste(out_of_range, collapse = ", ")))
  }
  expect_true(all(pollscore_data$pollscore[!is.na(pollscore_data$pollscore)] >= 0 &
                    pollscore_data$pollscore[!is.na(pollscore_data$pollscore)] <= 100),
              "Poll score is out of the expected range (0-100).")
})


test_that("Column Type Test: All columns have expected data types", {
  pollscore_data <- read_csv(here("data", "02-analysis_data", "num_grade_pollscore_data.csv"))
  print(names(pollscore_data))  # Debugging: Print column names to verify
  expect_true(is.numeric(pollscore_data$numeric_grade), "numeric_grade is not numeric")
  expect_true(is.numeric(pollscore_data$pollscore), "pollscore is not numeric")
})


