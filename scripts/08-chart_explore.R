# Load necessary libraries
library(ggthemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)

# Load the cleaned data
cleaned_data <- read.csv("data/02-analysis_data/clean_data.csv")



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



