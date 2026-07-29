############################################################
# Project : Global NCD Intelligence Dashboard
# File    : 03_dashboard_datasets.R
# Author  : KIPKOECH PATRICK
# Course  : SDS 6103 - Statistical Computing
# Program : MSc Data Science
# Purpose : Create optimized datasets for the Shiny Dashboard
############################################################

# ==========================================================
# Install Required Packages (Run Once)
# ==========================================================

required_packages <- c(
  "readr",
  "dplyr",
  "tidyr",
  "countrycode"
)

new_packages <- required_packages[
  !(required_packages %in% installed.packages()[,"Package"])
]

if(length(new_packages) > 0){
  install.packages(new_packages)
}

# ==========================================================
# Load Packages
# ==========================================================

library(readr)
library(dplyr)
library(tidyr)
library(countrycode)

# ==========================================================
# Import Datasets
# ==========================================================

country_data <- read_csv("data/country_data.csv")
regional_data <- read_csv("data/regional_data.csv")

# ==========================================================
# Remove OWID Aggregates (if any)
# ==========================================================

country_data <- country_data %>%
  filter(!grepl("^OWID", Code))

# ==========================================================
# Add Continents
# ==========================================================

country_data <- country_data %>%
  mutate(
    Continent = countrycode(
      Code,
      origin = "iso3c",
      destination = "continent"
    )
  )

# ==========================================================
# Executive Dashboard Dataset
# ==========================================================

executive_summary <- country_data %>%
  summarise(
    Countries = n_distinct(Entity),
    Years = n_distinct(Year),
    Total_DALYs = sum(Diabetes_Kidney_DALYs),
    Mean_DALYs = mean(Diabetes_Kidney_DALYs),
    Median_DALYs = median(Diabetes_Kidney_DALYs),
    Maximum_DALYs = max(Diabetes_Kidney_DALYs)
  )

# ==========================================================
# Latest World Map Dataset
# ==========================================================

map_data <- country_data %>%
  filter(Year == max(Year)) %>%
  select(
    Entity,
    Code,
    Continent,
    Year,
    Diabetes_Kidney_DALYs
  )

# ==========================================================
# Country Rankings
# ==========================================================

country_rankings <- country_data %>%
  group_by(Entity, Code, Continent) %>%
  summarise(
    Total_DALYs = sum(Diabetes_Kidney_DALYs),
    Mean_DALYs = mean(Diabetes_Kidney_DALYs),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_DALYs))

# ==========================================================
# Country Time Series
# ==========================================================

country_timeseries <- country_data %>%
  select(
    Entity,
    Code,
    Continent,
    Year,
    Diabetes_Kidney_DALYs
  )

# ==========================================================
# Global Disease Summary
# ==========================================================

global_disease_summary <- country_data %>%
  summarise(
    Cirrhosis = sum(Cirrhosis_Liver_DALYs),
    Mental = sum(Mental_Disorders_DALYs),
    Respiratory = sum(Chronic_Respiratory_DALYs),
    Neurological = sum(Neurological_DALYs),
    Cardiovascular = sum(Cardiovascular_DALYs),
    Skin = sum(Skin_DALYs),
    Substance = sum(Substance_Use_DALYs),
    Musculoskeletal = sum(Musculoskeletal_DALYs),
    Neoplasms = sum(Neoplasms_DALYs),
    Digestive = sum(Digestive_DALYs),
    Other_NCDs = sum(Other_NCDs_DALYs),
    Diabetes_Kidney = sum(Diabetes_Kidney_DALYs)
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Disease",
    values_to = "Total_DALYs"
  ) %>%
  mutate(
    Percentage = round(
      Total_DALYs /
        sum(Total_DALYs) * 100,
      2
    )
  ) %>%
  arrange(desc(Total_DALYs))

# ==========================================================
# Global Diabetes Trend
# ==========================================================

global_diabetes_trend <- country_data %>%
  group_by(Year) %>%
  summarise(
    Total_DALYs = sum(Diabetes_Kidney_DALYs),
    .groups = "drop"
  )

# ==========================================================
# Top 10 Countries (Latest Year)
# ==========================================================

top10_2019 <- country_data %>%
  filter(Year == max(Year)) %>%
  arrange(desc(Diabetes_Kidney_DALYs)) %>%
  select(
    Entity,
    Code,
    Continent,
    Diabetes_Kidney_DALYs
  ) %>%
  slice_head(n = 10)

# ==========================================================
# Country Growth Dataset
# ==========================================================

country_growth <- country_data %>%
  filter(Year %in% c(1990, 2019)) %>%
  select(
    Entity,
    Code,
    Continent,
    Year,
    Diabetes_Kidney_DALYs
  ) %>%
  pivot_wider(
    names_from = Year,
    values_from = Diabetes_Kidney_DALYs
  ) %>%
  mutate(
    Absolute_Increase = `2019` - `1990`,
    Percentage_Increase =
      ((`2019` - `1990`) / `1990`) * 100
  ) %>%
  arrange(desc(Absolute_Increase))

# ==========================================================
# Continent Summary Dataset
# ==========================================================

continent_summary <- country_data %>%
  group_by(Continent) %>%
  summarise(
    Countries = n_distinct(Entity),
    Total_DALYs = sum(Diabetes_Kidney_DALYs),
    Mean_DALYs = mean(Diabetes_Kidney_DALYs),
    Median_DALYs = median(Diabetes_Kidney_DALYs),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_DALYs))


# ==========================================================
# Continent Trend Dataset
# ==========================================================

continent_trend <- country_data %>%
  group_by(Continent, Year) %>%
  summarise(
    Total_DALYs = sum(Diabetes_Kidney_DALYs),
    .groups = "drop"
  )


# ==========================================================
# WHO Regional Dataset
# ==========================================================

who_regions <- regional_data %>%
  filter(grepl("\\(WHO\\)", Entity))


# ==========================================================
# WHO Regional Summary
# ==========================================================

who_summary <- who_regions %>%
  group_by(Entity) %>%
  summarise(
    Total_DALYs = sum(Diabetes_Kidney_DALYs),
    Mean_DALYs = mean(Diabetes_Kidney_DALYs),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_DALYs))


# ==========================================================
# WHO Regional Trend Dataset
# ==========================================================

who_trend <- who_regions %>%
  group_by(Entity, Year) %>%
  summarise(
    Total_DALYs = sum(Diabetes_Kidney_DALYs),
    .groups = "drop"
  )


# ==========================================================
# Save Dashboard Datasets
# ==========================================================

write_csv(
  executive_summary,
  "data/executive_summary.csv"
)

write_csv(
  map_data,
  "data/map_data.csv"
)

write_csv(
  country_rankings,
  "data/country_rankings.csv"
)

write_csv(
  country_timeseries,
  "data/country_timeseries.csv"
)

write_csv(
  global_disease_summary,
  "data/global_disease_summary.csv"
)

write_csv(
  global_diabetes_trend,
  "data/global_diabetes_trend.csv"
)

write_csv(
  top10_2019,
  "data/top10_2019.csv"
)

write_csv(
  country_growth,
  "data/country_growth.csv"
)

write_csv(
  continent_summary,
  "data/continent_summary.csv"
)

write_csv(
  continent_trend,
  "data/continent_trend.csv"
)

write_csv(
  who_summary,
  "data/who_summary.csv"
)

write_csv(
  who_trend,
  "data/who_trend.csv"
)


# ==========================================================
# Validation
# ==========================================================

cat("\n")
cat("============================================\n")
cat("Dashboard datasets successfully created\n")
cat("============================================\n")

cat(
  "Executive Summary     :",
  nrow(executive_summary),
  "row(s)\n"
)

cat(
  "Map Data              :",
  nrow(map_data),
  "rows\n"
)

cat(
  "Country Rankings      :",
  nrow(country_rankings),
  "rows\n"
)

cat(
  "Country Time Series   :",
  nrow(country_timeseries),
  "rows\n"
)

cat(
  "Disease Summary       :",
  nrow(global_disease_summary),
  "rows\n"
)

cat(
  "Global Trend          :",
  nrow(global_diabetes_trend),
  "rows\n"
)

cat(
  "Top 10 Countries      :",
  nrow(top10_2019),
  "rows\n"
)

cat(
  "Country Growth        :",
  nrow(country_growth),
  "rows\n"
)

cat(
  "Continent Summary     :",
  nrow(continent_summary),
  "rows\n"
)

cat(
  "Continent Trend       :",
  nrow(continent_trend),
  "rows\n"
)

cat(
  "WHO Summary           :",
  nrow(who_summary),
  "rows\n"
)

cat(
  "WHO Trend             :",
  nrow(who_trend),
  "rows\n"
)

cat("============================================\n\n")

list.files("data", pattern = "\\.csv$")