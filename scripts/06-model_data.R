#### Preamble ####
# Purpose: TBD
# Author: Ziheng Zhong
# Date: 24 October 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT



#### Workspace setup ####
library(modelsummary)
library(randomForest)
library(rstanarm)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)



#### Bayesian Linear Regression Model for Support Prediction ####
# Load the dataset
data <- read.csv("data/02-analysis_data/candidate_sup_num_data.csv")

# Convert the 'date' column to Date type if needed
data$date <- as.Date(data$date, format="%m/%d/%y")

# Build the linear model
linear_model <- stan_glm(support_pct ~ weight_value, data=data, family=gaussian(), seed=1234)

# Summarize the linear model
summary(linear_model)

# Visualize the model using posterior predictive checks
library(bayesplot)
sim_run_data_second_model_rstanarm <- posterior_predict(linear_model)

# Plot posterior predictive check
pp_plot <- pp_check(linear_model) +
  theme_classic() +
  theme(legend.position = "bottom")

print(pp_plot)



#### Random Forest Model for Predicting Support Percentage ####
# Load the new dataset
data_new <- read.csv("data/02-analysis_data/num_grade_pollscore_data.csv")

# Convert the 'date' column to Date type if needed
data_new$date <- as.Date(data_new$date, format="%m/%d/%y")

# Build the Random Forest model
random_forest_model <- randomForest(support_pct ~ numeric_grade + pollscore, data=data_new, ntree=500, mtry=2, importance=TRUE, seed=1234)

# Summarize the Random Forest model
print(random_forest_model)

# Plot the importance of variables
varImpPlot(random_forest_model)

print(random_forest_model)


