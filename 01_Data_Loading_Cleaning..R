# ==============================================================
# GLOBAL DIABETES & KIDNEY DISEASE BURDEN
# STEP 1: DATA LOADING & CLEANING
# ==============================================================

library(tidyverse)
library(janitor)

# Load raw dataset
data_raw <- read_csv(
  "disease-burden-from-ncds.csv",
  show_col_types = FALSE
)

# Clean column names
data_clean <- data_raw %>%
  clean_names()

# Select and rename required variables
diabetes_data <- data_clean %>%
  select(
    entity,
    code,
    year,
    dal_ys_disability_adjusted_life_years_diabetes_and_kidney_diseases_sex_both_age_all_ages_number
  ) %>%
  rename(
    country = entity,
    country_code = code,
    daly = dal_ys_disability_adjusted_life_years_diabetes_and_kidney_diseases_sex_both_age_all_ages_number
  ) %>%
  mutate(
    year = as.integer(year),
    daly = as.numeric(daly)
  ) %>%
  filter(
    !is.na(country),
    !is.na(year),
    !is.na(daly)
  )

# Create data folder if needed
if (!dir.exists("data")) dir.create("data")

# Save cleaned dataset
write_csv(diabetes_data, "data/diabetes_clean.csv")

cat("Data cleaning complete.\n")
cat("Rows:", nrow(diabetes_data), "\n")
cat("Countries:", n_distinct(diabetes_data$country), "\n")
cat("Year range:", min(diabetes_data$year), "-", max(diabetes_data$year), "\n")