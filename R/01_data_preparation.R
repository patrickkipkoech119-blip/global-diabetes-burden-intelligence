############################################################
# Project : Global NCD Intelligence Dashboard
# File    : 01_data_preparation.R
# Author  : KIPKOECH PATRICK
# Course  : SDS 6103 - Statistical Computing
# Program : MSc Data Science
# Purpose : Import, inspect, validate, clean, and prepare
#           the Global NCD Burden dataset for dashboard
#           analysis.
# Date    : 21 July 2026
############################################################


# ==========================================================
# Install Required Packages (Run Once)
# ==========================================================

required_packages <- c(
  "readr",
  "dplyr",
  "tidyr",
  "stringr",
  "janitor",
  "countrycode",
  "skimr",
  "naniar"
)

new_packages <- required_packages[
  !(required_packages %in% installed.packages()[, "Package"])
]

if(length(new_packages) > 0){
  install.packages(new_packages)
}


# ==========================================================
# Load Required Packages
# ==========================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(countrycode)
library(skimr)
library(naniar)


# ==========================================================
# Import Dataset
# ==========================================================

ncd_data <- read_csv("data/ncd_burden_raw.csv")


# ==========================================================
# Initial Data Inspection
# ==========================================================

glimpse(ncd_data)
str(ncd_data)


# ==========================================================
# Basic Data Quality Checks
# ==========================================================

# Dataset dimensions
dim(ncd_data)

# Column names
names(ncd_data)

# Summary statistics
summary(ncd_data)

# Missing values
colSums(is.na(ncd_data))

# Duplicate records
sum(duplicated(ncd_data))

# Number of unique entities
n_distinct(ncd_data$Entity)

# Number of unique years
n_distinct(ncd_data$Year)

# Study period
range(ncd_data$Year)


# ==========================================================
# Investigate Missing ISO Codes
# ==========================================================

missing_code_entities <-
  ncd_data %>%
  filter(is.na(Code)) %>%
  distinct(Entity)

missing_code_entities


# ==========================================================
# Rename Variables
# ==========================================================

names(ncd_data) <- c(
  "Entity",
  "Code",
  "Year",
  "Cirrhosis_Liver_DALYs",
  "Mental_Disorders_DALYs",
  "Chronic_Respiratory_DALYs",
  "Neurological_DALYs",
  "Cardiovascular_DALYs",
  "Skin_DALYs",
  "Substance_Use_DALYs",
  "Musculoskeletal_DALYs",
  "Neoplasms_DALYs",
  "Digestive_DALYs",
  "Other_NCDs_DALYs",
  "Diabetes_Kidney_DALYs"
)

# Verify renamed variables
names(ncd_data)


# ==========================================================
# Create Clean Analytical Dataset
# ==========================================================

ncd_data_clean <- ncd_data

# Verify cleaned dataset
glimpse(ncd_data_clean)


# ==========================================================
# Create Country Dataset (Countries Only)
# ==========================================================

country_data <- ncd_data_clean %>%
  filter(
    !is.na(Code),
    Code != "OWID_WRL"
  )

# ==========================================================
# Create Regional Dataset
# ==========================================================

regional_data <- ncd_data_clean %>%
  filter(is.na(Code))


# ==========================================================
# Verify Dataset Dimensions
# ==========================================================

cat("\nDataset Dimensions\n")
cat("------------------\n")

cat("Original Dataset : ", dim(ncd_data)[1], "rows x", dim(ncd_data)[2], "columns\n")
cat("Country Dataset  : ", dim(country_data)[1], "rows x", dim(country_data)[2], "columns\n")
cat("Regional Dataset : ", dim(regional_data)[1], "rows x", dim(regional_data)[2], "columns\n")


# ==========================================================
# Save Clean Datasets
# ==========================================================

write_csv(ncd_data_clean, "data/ncd_data_clean.csv")
write_csv(country_data, "data/country_data.csv")
write_csv(regional_data, "data/regional_data.csv")


# ==========================================================
# Completion Message
# ==========================================================

cat("\n")
cat("==============================================\n")
cat(" Data Preparation Completed Successfully\n")
cat("==============================================\n")
cat("Clean datasets have been saved to:\n")
cat(" - data/ncd_data_clean.csv\n")
cat(" - data/country_data.csv\n")
cat(" - data/regional_data.csv\n")
cat("==============================================\n")