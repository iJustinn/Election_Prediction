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
  select(state, pollster_rating_name, pollscore, candidate_name, pct, sample_size, start_date, end_date) %>%
  
  # Dropping rows where critical information is missing
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

# Save the cleaned data
write_csv(clean_data, here("data", "02-analysis_data", "clean_data.csv"))


