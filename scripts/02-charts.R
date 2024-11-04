#### Preamble ####
# Purpose: This code generates various visualizations comparing support percentages over time and by state for candidates Kamala Harris and Donald Trump, including weighted and unweighted support trends, numeric grade adjustments, and state-by-state mappings of average polling support and leading candidates.
# Author: Yingke He, Ziheng Zhong
# Date: 20 October 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT



#### Workspace setup ####
library(ggthemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)



# Load data
cleaned_data <- read.csv("data/02-analysis_data/clean_data.csv")
candidate_sup_num_data <- read.csv("data/02-analysis_data/candidate_sup_num_data.csv")
num_grade_pollscore_data <- read_csv("data/02-analysis_data/num_grade_pollscore_data.csv")



#### candidate_sup_num_data Chart ####
# Plot the weighted percentage change over time for both candidates
candidate_sup_num_data %>%
  ggplot(aes(x = as.Date(date, format = "%m/%d/%y"), y = support_pct * weight_value, color = candidate)) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_manual(values = c("Trump" = "blue", "Harris" = "red")) +
  labs(
    title = "Weighted Support Percentage Change Over Time",
    x = "Date",
    y = "Weighted Support Percentage",
    color = "Candidate"
  ) +
  theme_minimal()



# Plot the unweighted percentage change over time for both candidates
candidate_sup_num_data %>%
  ggplot(aes(x = as.Date(date, format = "%m/%d/%y"), y = support_pct, color = candidate)) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_manual(values = c("Trump" = "blue", "Harris" = "red")) +
  labs(
    title = "Unweighted Support Percentage Change Over Time",
    x = "Date",
    y = "Support Percentage",
    color = "Candidate"
  ) +
  theme_minimal()



#### num_grade_pollscore_data Chart ####
# Convert the 'date' column to Date type
num_grade_pollscore_data %>%
  mutate(date = as.Date(date, format = '%m/%d/%y')) %>%
  
  # Summarize data by month and candidate to reduce the number of points
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month, candidate) %>%
  summarize(
    support_pct = mean(support_pct),
    pollscore = mean(pollscore),
    count = n(),
    .groups = 'drop'
  ) %>%
  
  # Plotting numeric_grade as weight using scatter plot with size representing amount of data
  ggplot(aes(x = as.Date(paste0(month, "-01")), y = support_pct, color = candidate, size = count)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("Harris" = "red", "Trump" = "blue")) +
  labs(
    title = 'Support Rate Over Time (Weighted by Numeric Grade)',
    x = 'Date',
    y = 'Support Rate (%)',
    color = 'Candidate',
    size = 'Number of Data Points'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plotting using pollscore with scatter plot with size representing amount of data
summarized_data %>%
  ggplot(aes(x = as.Date(paste0(month, "-01")), y = pollscore, color = candidate, size = count)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("Harris" = "red", "Trump" = "blue")) +
  labs(
    title = 'Support Rate Over Time (Based on Pollscore)',
    x = 'Date',
    y = 'Poll Score',
    color = 'Candidate',
    size = 'Number of Data Points'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#### Harris Chart ####
# Summarize polling percentages by state and candidate
state_summary <- cleaned_data %>%
  filter(candidate_name == "Kamala Harris") %>%  # Choose a candidate (you can change or generalize this)
  group_by(state) %>%
  summarize(avg_pct = mean(pct, na.rm = TRUE))

# Prepare US map data
us_states <- map_data("state")

# Convert state names to lowercase for matching with map data
state_summary$state <- tolower(state_summary$state)

# Merge state summary with map data
merged_data <- left_join(us_states, state_summary, by = c("region" = "state"))

# Plot the average polling percentage for Kamala Harris by state on a US map
ggplot(merged_data, aes(x = long, y = lat, group = group, fill = avg_pct)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Avg % Support", na.value = "grey90") +
  labs(title = "Average Polling Support for Kamala Harris by State",
       x = "",
       y = "") +
  theme_map()



#### Trump Chart ####
# Summarize polling percentages by state and candidate
state_summary_trump <- cleaned_data %>%
  filter(candidate_name == "Donald Trump") %>%  # Choose a candidate (you can change or generalize this)
  group_by(state) %>%
  summarize(avg_pct = mean(pct, na.rm = TRUE))

# Convert state names to lowercase for matching with map data
state_summary_trump$state <- tolower(state_summary_trump$state)

# Merge state summary with map data
merged_data_trump <- left_join(us_states, state_summary_trump, by = c("region" = "state"))

# Plot the average polling percentage for Donald Trump by state on a US map
ggplot(merged_data_trump, aes(x = long, y = lat, group = group, fill = avg_pct)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "lightpink", high = "darkred", name = "Avg % Support", na.value = "grey90") +
  labs(title = "Average Polling Support for Donald Trump by State",
       x = "",
       y = "") +
  theme_map()



#### Mixed Chart ####
# Summarize polling percentages by state and candidate
state_summary_mixed <- cleaned_data %>%
  filter(candidate_name %in% c("Kamala Harris", "Donald Trump")) %>%
  group_by(state, candidate_name) %>%
  summarize(avg_pct = mean(pct, na.rm = TRUE)) %>%
  pivot_wider(names_from = candidate_name, values_from = avg_pct, values_fill = 0)

# Determine the leading candidate for each state
state_summary_mixed <- state_summary_mixed %>%
  mutate(leading_candidate = case_when(
    `Kamala Harris` > 45 ~ "Harris",
    `Donald Trump` > 45 ~ "Trump",
    TRUE ~ "Tied"
  ))

# Convert state names to lowercase for matching with map data
state_summary_mixed$state <- tolower(state_summary_mixed$state)

# Merge state summary with map data
merged_data_mixed <- left_join(us_states, state_summary_mixed, by = c("region" = "state"))

# Plot the support for Kamala Harris and Donald Trump by state on a US map
ggplot(merged_data_mixed, aes(x = long, y = lat, group = group, fill = leading_candidate)) +
  geom_polygon(color = "black") +
  scale_fill_manual(values = c("Trump" = "blue", "Harris" = "red", "Tied" = "lightgrey"), name = "Leading Candidate") +
  labs(title = "States with 45%+ Support for Each Candidate",
       x = "",
       y = "") +
  theme_map()


