#### Preamble ####
# Purpose: TBD
# Author: Ziheng Zhong
# Date: 24 October 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT



#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(here)



#### Clean data ####
# Data #1
# Load data
raw_data <- read_csv(here("data", "01-raw_data", "president_polls.csv"))

# Filtering
candidate_sup_num_data <- raw_data %>%
  filter(answer %in% c("Trump", "Harris"), election_date == '11/5/24') %>%
  mutate(weight_value = log(sample_size)) %>%
  select(candidate = answer, weight_value, support_pct = pct, date = end_date)

# Save data
write_csv(candidate_sup_num_data, "data/02-analysis_data/candidate_sup_num_data.csv")



# Data #2
# Load data
raw_data <- read_csv(here("data", "01-raw_data", "president_polls.csv"))

# Filtering
num_grade_pollscore_data <- raw_data %>%
  filter(answer %in% c("Trump", "Harris"), election_date == '11/5/24') %>%
  filter(!is.na(numeric_grade), !is.na(pollscore), pollscore >= 0) %>%
  select(candidate = answer, date = end_date, support_pct = pct, numeric_grade, pollscore)

# Save data
write_csv(num_grade_pollscore_data, "data/02-analysis_data/num_grade_pollscore_data.csv")



# Data #3
# Load data
raw_data <- read_csv(here("data", "01-raw_data", "president_polls.csv"))

# Select essential columns
clean_data <- raw_data %>%

  select(state, pollster_rating_name, pollscore, candidate_name, pct, sample_size, start_date, end_date)

# Dropping rows where critical information like candidate name or percentage is missing
clean_data <- clean_data %>%
  filter(!is.na(state)) %>%
  filter(!is.na(pollster_rating_name)) %>%
  filter(!is.na(pollscore)) %>%
  filter(!is.na(candidate_name)) %>%
  filter(!is.na(pct)) %>%
  filter(!is.na(sample_size)) %>%
  filter(!is.na(start_date)) %>%
  filter(!is.na(end_date)) %>%
  
  # Filter to keep only rows with candidates "Kamala Harris" or "Donald Trump"
  filter(candidate_name %in% c("Kamala Harris", "Donald Trump"))

# Check the first few rows of clean_data
head(clean_data)

# Set election date
election_date <- as.Date("2024-11-05")

# Calculate days to election without modifying the end_date format
clean_data <- clean_data %>%
  mutate(temp_end_date = as.Date(end_date, format = "%m/%d/%y")) %>%
  mutate(days_to_election = as.numeric(difftime(election_date, temp_end_date, units = "days"))) %>%
  select(-temp_end_date) # Remove temporary column

# Save data
write_csv(clean_data, here("data", "02-analysis_data", "clean_data.csv"))



# Data #4
# Load data
data <- read_csv(here("data", "02-analysis_data", "clean_data.csv"), show_col_types = FALSE)

# Calculate the average state effect based on polling percentage (pct) by state and pollster_rating_name
state_effects <- data %>%
  group_by(state, pollster_rating_name) %>%
  summarise(
    state_effect = round(mean(pct, na.rm = TRUE), 1)  # Average polling percentage for each state-pollster combination, rounded to 1 decimal
  )

# Save data
write_csv(state_effects, path = here("data", "02-analysis_data", "state_effect.csv"))



# Data #5
# Read in the cleaned data
data <- read_csv(here("data", "02-analysis_data", "clean_data.csv"), show_col_types = FALSE)

# Calculate the candidate fixed effect as the average support across all polls
candidate_fixed_effect <- data %>%
  group_by(candidate_name) %>%
  summarise(gamma_k = round(mean(pct, na.rm = TRUE), 1))

# Display candidate fixed effects
#print(candidate_fixed_effect)

# Save candidate_fixed_effect to a CSV file
write_csv(candidate_fixed_effect, path = here("data", "02-analysis_data", "candidate_fixed_effect.csv"))



# Data #6
# Read in the cleaned data
data <- read_csv(here("data", "02-analysis_data", "clean_data.csv"), show_col_types = FALSE)

# Ensure the pollscore and sample_size columns are non-NA (replace NAs with default values if needed)
data <- data %>%
  mutate(
    pollscore = ifelse(is.na(pollscore), 0, pollscore),  # Replace NA pollscore with 0
    sample_size = ifelse(is.na(sample_size), 1, sample_size)  # Replace NA sample_size with 1
  )

# Calculate the weight for each poll as the product of pollscore and sample_size
data <- data %>%
  mutate(weight = pollscore * sample_size)

# Calculate the weighted average of pct for each candidate in each state
weighted_predictions <- data %>%
  group_by(state, candidate_name) %>%
  summarise(
    weighted_pct = round(sum(pct * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE), 0),
    total_weight = round(sum(weight, na.rm = TRUE), 0)  # Optional: To inspect total weight per group, rounded to 0 dp
  ) %>%
  ungroup()

# Save weighted_predictions to an csv file
write_csv(weighted_predictions, path = here("data", "02-analysis_data", "weighted_predictions.csv"))



# Data #7 & 8
# Read in the cleaned data
data <- read_csv(here("data", "02-analysis_data", "clean_data.csv"), show_col_types = FALSE)

# Ensure no NA values in pollscore, sample_size, and days_to_election
data <- data %>%
  mutate(
    pollscore = ifelse(is.na(pollscore), 0, pollscore),  
    sample_size = ifelse(is.na(sample_size), 1, sample_size),  
    days_to_election = ifelse(is.na(days_to_election) | days_to_election <= 0, 
                              max(days_to_election, na.rm = TRUE), 
                              days_to_election)  
  )

# Calculate recency weight
data <- data %>%
  mutate(recency_weight = ifelse(days_to_election > 0, 1 / days_to_election, 0))

# Calculate total weight
data <- data %>%
  mutate(total_weight = pollscore * sample_size * recency_weight)

# Calculate the weighted average of pct for each candidate in each state
weighted_predictions <- data %>%
  group_by(state, candidate_name) %>%
  summarise(
    weighted_pct = round(ifelse(sum(total_weight, na.rm = TRUE) == 0, 0, 
                                sum(pct * total_weight, na.rm = TRUE) / sum(total_weight, na.rm = TRUE)), 1),
    total_weight = round(sum(total_weight, na.rm = TRUE), 1)
  ) %>%
  ungroup()

# Save weighted predictions to csv
write_csv(weighted_predictions, path = here("data", "02-analysis_data", "weighted_predictions_with_recency.csv"))

# Pollster reliability adjustment
zeta <- 0.2  
data <- data %>%
  mutate(
    pollster_reliability_adjustment = round(zeta * pollscore, 1)  # Rounded to 1 decimal place
  )

# Save pollster reliability adjustments to csv
write_csv(data, path = here("data", "02-analysis_data", "pollscore_reliability_adjust.csv"))



# Data #9
# Read in the cleaned data
data <- read_csv(here("data", "02-analysis_data", "clean_data.csv"), show_col_types = FALSE)

# Check if the column names are as expected
#print(colnames(data))

# Calculate the average pollster effect based on pollscore
pollster_effects <- data %>%
  group_by(pollster_rating_name) %>%
  summarise(
    pollster_effect = mean(pollscore, na.rm = TRUE)  # Calculate average pollscore for each pollster
  )

# Display the pollster effects
#print(pollster_effects)

# Save pollster effects to a CSV file, including the pollster_rating_name
write_csv(pollster_effects, path = here("data", "02-analysis_data", "pollster_effect.csv"))



