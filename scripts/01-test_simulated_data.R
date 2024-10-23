#### Preamble ####
# Purpose: Tests the structure and validity of the simulated Australian 
  #electoral divisions dataset.
# Author: Rohan Alexander
# Date: 26 September 2024
# Contact: rohan.alexander@utoronto.ca
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded
  # - 00-simulate_data.R must have been run
# Any other information needed? Make sure you are in the `starter_folder` rproj


#### Workspace setup ####
# Load Libraries
library(tidyverse)
library(testthat)
library(tibble)
library(dplyr)
library(here)



#### Test cases ####
test_that("Test for number of states matches the predefined state list", {
  simulated_data <- read_csv(here("data", "00-simulated_data", "simulated_data.csv"))
  states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
  expect_equal(length(unique(simulated_data$state)), length(states),
               info = "The number of unique states in the dataset does not match the predefined state list.")
})

test_that("Test for voter ID uniqueness", {
  simulated_data <- read_csv(here("data", "00-simulated_data", "simulated_data.csv"))
  expect_equal(length(unique(simulated_data$voter_id)), nrow(simulated_data),
               info = "Voter IDs are not unique.")
})

test_that("Test for age values within valid range (18 to 100)", {
  simulated_data <- read_csv(here("data", "00-simulated_data", "simulated_data.csv"))
  expect_true(all(simulated_data$age >= 18 & simulated_data$age <= 100),
              info = "Age values are outside the valid range of 18 to 100.")
})

test_that("Test for valid gender values", {
  simulated_data <- read_csv(here("data", "00-simulated_data", "simulated_data.csv"))
  expect_true(all(simulated_data$gender %in% c("Male", "Female", "Other")),
              info = "Gender values contain invalid entries.")
})

test_that("Test for valid candidate preference values", {
  simulated_data <- read_csv(here("data", "00-simulated_data", "simulated_data.csv"))
  candidates <- c("Harris", "Trump")
  expect_true(all(simulated_data$candidate_preference %in% candidates),
              info = "Candidate preference values contain invalid entries.")
})

test_that("Test for number of votes greater than zero for each state and candidate", {
  simulated_data <- read_csv(here("data", "00-simulated_data", "simulated_data.csv"))
  election_results <- simulated_data %>%
    group_by(state, candidate_preference) %>%
    summarise(votes = n(), .groups = 'drop')
  expect_true(all(election_results$votes > 0),
              info = "There are states or candidates with zero votes.")
})

test_that("Test for each state having votes for both candidates", {
  simulated_data <- read_csv(here("data", "00-simulated_data", "simulated_data.csv"))
  candidates <- c("Harris", "Trump")
  election_results <- simulated_data %>%
    group_by(state, candidate_preference) %>%
    summarise(votes = n(), .groups = 'drop')
  states_with_both_candidates <- election_results %>%
    group_by(state) %>%
    summarise(candidate_count = n()) %>%
    filter(candidate_count == length(candidates))
  expect_equal(nrow(states_with_both_candidates), length(unique(simulated_data$state)),
               info = "Not all states have votes for both candidates.")
})


