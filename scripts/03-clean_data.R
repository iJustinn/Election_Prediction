#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 6 April 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

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
  filter(!is.na(numeric_grade), !is.na(pollscore)) %>%
  select(candidate = answer, date = end_date, support_pct = pct, numeric_grade, pollscore)

# Save data
write_csv(num_grade_pollscore_data, "data/02-analysis_data/num_grade_pollscore_data.csv")









