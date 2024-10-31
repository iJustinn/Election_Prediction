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

# Keeping columns that are essential for analysis
clean_data <- raw_data %>%
  select(state, pollster_rating_name, candidate_name, pct, sample_size, start_date, end_date)

# Dropping rows where critical information like candidate name or percentage is missing
clean_data <- clean_data %>%
  filter(!is.na(state)) %>%
  filter(!is.na(pollster_rating_name)) %>%
  filter(!is.na(candidate_name)) %>%
  filter(!is.na(pct)) %>%
  filter(!is.na(sample_size)) %>%
  filter(!is.na(start_date)) %>%
  filter(!is.na(end_date))

# Calculate the number of days to the election date (November 5th) without modifying end_date format
election_date <- as.Date("2024-11-05")
clean_data <- clean_data %>%
  mutate(temp_end_date = as.Date(end_date, format = "%m/%d/%y")) %>%
  mutate(days_to_election = as.numeric(difftime(election_date, temp_end_date, units = "days"))) %>%
  select(-temp_end_date)

# Save the cleaned data
write_csv(clean_data, "data/02-analysis_data/clean_data.csv")


