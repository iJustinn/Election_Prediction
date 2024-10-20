#### Preamble ####
# Purpose: TBD
# Author: Ziheng Zhong
# Date: 20 October 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT



#### Workspace setup ####
library(tidyverse)
set.seed(304)



#### Simulate data ####
# Defining the states and candidates
states <- state.name
candidates <- c("Trump", "Harris")

# Number of voters per state (randomly selected between 500,000 to 10,000,000)
num_voters <- sample(500:1000, length(states), replace = TRUE)

# Simulate data frame for voters in each state
analysis_data <- tibble(
  state = rep(states, num_voters),
  voter_id = 1:sum(num_voters),
  age = sample(18:100, sum(num_voters), replace = TRUE),
  gender = sample(c("Male", "Female", "Other"), sum(num_voters), replace = TRUE, prob = c(0.48, 0.48, 0.04)),
  candidate_preference = sample(candidates, sum(num_voters), replace = TRUE, prob = c(0.5, 0.5))
)

# Aggregate the number of votes per candidate per state
election_results <- analysis_data %>%
  group_by(state, candidate_preference) %>%
  summarise(votes = n(), .groups = 'drop')



#### Save data ####
write_csv(analysis_data, "data/00-simulated_data/simulated_data.csv")


