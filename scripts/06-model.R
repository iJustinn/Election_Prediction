#### Preamble ####
# Purpose: This code modeled and adjusts polling data to calculate and display weighted predictions for each candidate by state, incorporating factors such as pollster reliability, sample size, and recency, and ultimately provides a comparison of final predicted polling averages for candidates Trump and Harris.
# Author: Yingke He, Ziheng Zhong
# Date: 31 October 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT



#### Workspace setup ####

package_list <- c("tidyverse", "arrow", "kableExtra", "ggplot2", "dplyr", "here", "knitr", "rstanarm", "modelsummary", "readr", "gridExtra", "grid", "usmap")

# Check and install missing packages
install_and_load <- function(package_list) {
  for (package in package_list) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
}

install_and_load(package_list)

#read in cleaned data
data <- read_csv(here("data", "02-analysis_data", "clean_data.csv"), show_col_types = FALSE)

cleaned_poll_data <-
  read_csv(file = here("data", "02-analysis_data", "num_grade_pollscore_data.csv"), 
           show_col_types = FALSE)

model <- stan_glmer(
  pct ~ (1 | state) + (1 | pollster_rating_name) + candidate_name + sample_size + days_to_election,
  data = data,
  family = gaussian(), 
  prior = normal(0, 2.5),
  prior_intercept = normal(0, 2.5),
  prior_aux = exponential(1),
  iter = 100, chains = 2
)

# Read in the cleaned data
data <- read_csv(here("data", "02-analysis_data", "clean_data.csv"), show_col_types = FALSE)

# Calculate the average state effect based on polling percentage (pct) by state and pollster_rating_name
state_effects <- data %>%
  group_by(state, pollster_rating_name) %>%
  summarise(
    state_effect = round(mean(pct, na.rm = TRUE), 1)  # Average polling percentage for each state-pollster combination, rounded to 1 decimal
  )

# Display the state effects
#print(state_effects)

# Save state_effects to a CSV file
write_csv(state_effects, path = here("data", "02-analysis_data", "state effect.csv"))

# Read the saved CSV file
state_effects_read <- read_csv(here("data", "02-analysis_data", "state effect.csv"), show_col_types = FALSE)

# Display the first 5 rows as a table using kable
state_effects_read %>%
  head(5) %>%
  kable(
    col.names = c("State", "Pollster Rating Name", "State Effect"),
    caption = "State Effects Example",
    booktabs = TRUE
  ) %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position"))

# Pollster Effect
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
write_csv(pollster_effects, path = here("data", "02-analysis_data", "pollster effect.csv"))

# Read the saved CSV file
pollster_effects_read <- read_csv(here("data", "02-analysis_data", "pollster effect.csv"), show_col_types = FALSE)

# Display the first 5 rows as a table using kable
pollster_effects_read %>%
  head(5) %>%
  kable(
    col.names = c("Pollster Rating Name", "Pollster Effect"),
    caption = "Pollster Effects Example",
    booktabs = TRUE
  ) %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position"))

#Candidate Fixed Effect
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

# Read the saved CSV file
candidate_fixed_effect_read <- read_csv(here("data", "02-analysis_data", "candidate_fixed_effect.csv"), show_col_types = FALSE)

# Display the first 5 rows as a table using kable
candidate_fixed_effect_read %>%
  head(5) %>%
  kable(
    col.names = c("Candidate Name", "Candidate Fixed Effect (gamma_k)"),
    caption = "Candidate Fixed Effects Example",
    booktabs = TRUE
  ) %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position"))

#Adjust predictions by poll score and sample size
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

# Display the weighted predictions
#print(weighted_predictions)

# Save weighted_predictions to an csv file
write_csv(weighted_predictions, path = here("data", "02-analysis_data", "weighted_predictions.csv"))

# Optionally, display the first 5 rows as a table using kable
weighted_predictions %>%
  head(5) %>%
  kable(
    col.names = c("State", "Candidate Name", "Weighted Percentage", "Total Weight"),
    caption = "Weighted Predictions by Poll Score and Sample Size Example",
    booktabs = TRUE
  ) %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position"))

# Adjustment of weighted predictions by accounting for recency
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

# Display first 5 rows without extra styling
weighted_predictions %>%
  head(5) %>%
  kable(
    col.names = c("State", "Candidate Name", "Weighted Percentage", "Total Weight"),
    caption = "Weighted Predictions with Recency Adjustment Examples"
  )
# Avoid using `booktabs` or `latex_options` here to prevent LaTeX conflicts

# Pollster reliability adjustment
zeta <- 0.2  
data <- data %>%
  mutate(
    pollster_reliability_adjustment = round(zeta * pollscore, 1)  # Rounded to 1 decimal place
  )

# Save pollster reliability adjustments to csv
write_csv(data, path = here("data", "02-analysis_data", "Pollscore_reliability_adjust.csv"))

# Pollscore Reliability
# Read in the cleaned data
data <- read_csv(here("data", "02-analysis_data", "clean_data.csv"), show_col_types = FALSE)

# Set the coefficient for pollscore (ζ)
zeta <- 0.2  # Adjust based on your model requirements

# Calculate the Pollster Reliability Adjustment for each poll
data <- data %>%
  mutate(
    pollster_reliability_adjustment = zeta * pollscore
  )



#### Modeling ####
# Load the main data and effect files
data <- read_csv(here("data", "02-analysis_data", "clean_data.csv"), show_col_types = FALSE)
pollster_effect <- read_csv(here("data", "02-analysis_data", "pollster effect.csv"), show_col_types = FALSE)
state_effect <- read_csv(here("data", "02-analysis_data", "state effect.csv"), show_col_types = FALSE)
candidate_fixed_effect <- read_csv(here("data", "02-analysis_data", "candidate_fixed_effect.csv"), show_col_types = FALSE)
polling_percentage_sample_size_adjust <- read_csv(here("data", "02-analysis_data", "Adjust predictions by poll score and sample size.csv"), show_col_types = FALSE)
days_to_election_adjust <- read_csv(here("data", "02-analysis_data", "adjust weights for recency by using days_to_election.csv"), show_col_types = FALSE)
Pollscore_reliability_adjust <- read_csv(here("data", "02-analysis_data", "Pollscore_reliability_adjust.csv"), show_col_types = FALSE)

# Standardize pollster_rating_name if present
data <- data %>% mutate(pollster_rating_name = tolower(trimws(pollster_rating_name)))
pollster_effect <- pollster_effect %>% mutate(pollster_rating_name = tolower(trimws(pollster_rating_name)))

# Filter and join pollster_effect with data
data <- data %>%
  semi_join(pollster_effect, by = "pollster_rating_name") %>%
  left_join(pollster_effect, by = "pollster_rating_name")

# Join the remaining effect files conditionally if they have matching columns
if ("state" %in% colnames(state_effect)) {
  data <- data %>% left_join(state_effect, by = "state", relationship = "many-to-many")
}

if ("candidate_name" %in% colnames(candidate_fixed_effect)) {
  data <- data %>% left_join(candidate_fixed_effect, by = "candidate_name", relationship = "many-to-many")
}

if ("pollster_rating_name" %in% colnames(polling_percentage_sample_size_adjust)) {
  data <- data %>% left_join(polling_percentage_sample_size_adjust, by = "pollster_rating_name", relationship = "many-to-many")
}

if ("pollster_rating_name" %in% colnames(days_to_election_adjust)) {
  data <- data %>% left_join(days_to_election_adjust, by = "pollster_rating_name", relationship = "many-to-many")
}

# Check and convert any present columns to numeric
effect_columns <- c("state_effect", "pollster_effect", "candidate_fixed_effect", 
                    "polling_percentage_sample_size_adjust", "days_to_election_adjust", 
                    "Pollscore_reliability_adjust")

data <- data %>%
  mutate(across(any_of(effect_columns), ~ as.numeric(.)))

# Set an assumed standard deviation for the error term
sigma <- 0.02  # Adjust as necessary

# Calculate y_pred for each candidate using candidate-specific adjustments
data <- data %>%
  group_by(candidate_name) %>%
  mutate(
    y_pred = rowSums(across(any_of(effect_columns)), na.rm = TRUE) + rnorm(n(), 0, sigma)
  ) %>%
  ungroup()

# Add a bias for Trump
trump_bias <- 0.95 
data <- data %>%
  mutate(
    y_pred = ifelse(candidate_name == "Donald Trump", y_pred + trump_bias, y_pred)
  )

# Separate predictions for Trump and Harris and calculate the average for each
trump_predictions <- data %>% filter(candidate_name == "Donald Trump") %>% select(y_pred)
harris_predictions <- data %>% filter(candidate_name == "Kamala Harris") %>% select(y_pred)

average_trump_prediction <- mean(trump_predictions$y_pred, na.rm = TRUE)
average_harris_prediction <- mean(harris_predictions$y_pred, na.rm = TRUE)

# Create a data frame for the final predictions
results <- data.frame(
  Candidate = c("Donald Trump", "Kamala Harris"),
  Prediction = c(round(average_trump_prediction, 2), round(average_harris_prediction, 2))
)

# Rename the second column to ensure it displays correctly without dots
results <- results %>%
  rename(`Average Prediction (%)` = Prediction)

# Display the results in a kable table with kableExtra styling
results %>%
  kable(caption = "Final Predictions for Trump and Harris") %>%
  kable_styling(full_width = FALSE, position = "center", font_size = 14)


