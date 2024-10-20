# Load necessary libraries
library(modelsummary)
library(randomForest)
library(gridExtra)
library(rstanarm)
library(ggplot2)
library(xgboost)
library(dplyr)
library(tidyr)
library(caret)

# Load the cleaned data
cleaned_data <- read.csv("data/01-raw_data/president_polls.csv")

# Prepare the data for modeling
# Filter the data for the two main candidates
model_data <- cleaned_data %>%
  filter(candidate_name %in% c("Kamala Harris", "Donald Trump")) %>%
  group_by(state, candidate_name) %>%
  summarize(avg_pct = mean(pct, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = candidate_name, values_from = avg_pct, values_fill = 0) %>%
  ungroup()

# Rename columns to remove spaces for compatibility
colnames(model_data) <- make.names(colnames(model_data))

# Add additional variables for linear modeling
# Use available features from the dataset
model_data <- model_data %>%
  left_join(cleaned_data %>% select(state, numeric_grade, sample_size, transparency_score) %>% distinct(), by = "state") %>%
  mutate(leading_candidate = ifelse(Kamala.Harris > Donald.Trump, 1, 0))

# Drop rows with NA values in the new features
model_data <- na.omit(model_data)

# Overall support percentage over time for Harris and Trump
support_over_time <- cleaned_data %>%
  filter(candidate_name %in% c("Kamala Harris", "Donald Trump"), as.Date(start_date, tryFormats = c("%Y-%m-%d", "%m/%d/%y")) >= as.Date("2024-07-01")) %>%
  group_by(start_date, candidate_name) %>%
  summarize(avg_pct = mean(pct, na.rm = TRUE), .groups = 'drop')

# Plot the overall support percentage over time for Kamala Harris
support_plot_harris <- ggplot(support_over_time %>% filter(candidate_name == "Kamala Harris", as.Date(start_date, tryFormats = c("%Y-%m-%d", "%m/%d/%y")) >= as.Date("2024-07-01")), aes(x = as.Date(start_date, tryFormats = c("%Y-%m-%d", "%m/%d/%y")), y = avg_pct)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(title = "Support Percentage Over Time for Kamala Harris",
       x = "Date",
       y = "Average Support Percentage") +
  theme_minimal()

print(support_plot_harris)

# Plot the overall support percentage over time for Donald Trump
support_plot_trump <- ggplot(support_over_time %>% filter(candidate_name == "Donald Trump"), aes(x = as.Date(start_date, tryFormats = c("%Y-%m-%d", "%m/%d/%y")), y = avg_pct)) +
  geom_line(color = "red", linewidth = 1) +
  labs(title = "Support Percentage Over Time for Donald Trump",
       x = "Date",
       y = "Average Support Percentage") +
  theme_minimal()

print(support_plot_trump)

# Fit linear models for each candidate separately
# Kamala Harris model
harris_model <- stan_glm(avg_pct ~ as.Date(start_date, tryFormats = c("%Y-%m-%d", "%m/%d/%y")), data = support_over_time %>% filter(candidate_name == "Kamala Harris"), refresh = 0)

# Summarize Harris model
modelsummary(harris_model)

# Posterior predictive check for Harris model
pp_check_harris <- pp_check(harris_model) +
  labs(title = "Posterior Predictive Check - Harris Model")
print(pp_check_harris)

# Donald Trump model
trump_model <- stan_glm(avg_pct ~ as.Date(start_date, tryFormats = c("%Y-%m-%d", "%m/%d/%y")), data = support_over_time %>% filter(candidate_name == "Donald Trump"), refresh = 0)

# Summarize Trump model
modelsummary(trump_model)

# Posterior predictive check for Trump model
pp_check_trump <- pp_check(trump_model) +
  labs(title = "Posterior Predictive Check - Trump Model")
print(pp_check_trump)

# Plot residuals for Harris model
residuals_plot_harris <- ggplot(harris_model, aes(.fitted, .resid)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Residuals vs Fitted - Harris Model", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Plot residuals for Trump model
residuals_plot_trump <- ggplot(trump_model, aes(.fitted, .resid)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Residuals vs Fitted - Trump Model", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Combine the residual plots using gridExtra
grid.arrange(residuals_plot_harris, residuals_plot_trump, ncol = 2)
