############################################################
# Project : Global NCD Intelligence Dashboard
# File    : 02_exploratory_analysis.R
# Author  : KIPKOECH PATRICK
# Course  : SDS 6103 - Statistical Computing
# Program : MSc Data Science
# Purpose : Explore and understand the cleaned Global NCD
#           Burden dataset to identify key insights for the
#           Shiny dashboard.
############################################################


# ==========================================================
# Install Required Packages (Run Once)
# ==========================================================

required_packages <- c(
  "readr",
  "dplyr",
  "tidyr",
  "ggplot2",
  "plotly",
  "skimr",
  "DT"
)

new_packages <- required_packages[
  !(required_packages %in% installed.packages()[,"Package"])
]

if(length(new_packages) > 0){
  install.packages(new_packages)
}


# ==========================================================
# Load Required Packages
# ==========================================================

library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(skimr)
library(DT)
library(tidyr)


# ==========================================================
# Import Clean Datasets
# ==========================================================

ncd_data <- read_csv("data/ncd_data_clean.csv")
country_data <- read_csv("data/country_data.csv")
regional_data <- read_csv("data/regional_data.csv")

# ==========================================================
# Dataset Overview
# ==========================================================

cat("\n")
cat("=========================================\n")
cat("       DATASET OVERVIEW\n")
cat("=========================================\n")

cat("Total observations       :", nrow(ncd_data), "\n")
cat("Total variables          :", ncol(ncd_data), "\n")
cat("Country observations     :", nrow(country_data), "\n")
cat("Regional observations    :", nrow(regional_data), "\n")
cat("Unique entities          :", n_distinct(ncd_data$Entity), "\n")
cat("Unique countries         :", n_distinct(country_data$Entity), "\n")
cat("Study period             :", min(ncd_data$Year), "-", max(ncd_data$Year), "\n")
cat("Number of disease groups :", ncol(ncd_data)-3, "\n")

cat("=========================================\n")

skim(country_data)

# ==========================================================
# Global Disease Burden Summary
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
      Total_DALYs / sum(Total_DALYs) * 100,
      2
    )
  ) %>%
  arrange(desc(Total_DALYs))

global_disease_summary

# ==========================================================
# Global Disease Burden Bar Chart
# ==========================================================

ggplot(
  global_disease_summary,
  aes(
    x = reorder(Disease, Total_DALYs),
    y = Total_DALYs
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Global Burden of Major NCD Groups (1990–2019)",
    x = "Disease Group",
    y = "Total DALYs"
  ) +
  theme_minimal()

# ==========================================================
# Top 20 Countries by Diabetes & Kidney Disease Burden
# ==========================================================

top20_diabetes <- country_data %>%
  group_by(Entity) %>%
  summarise(
    Total_DALYs = sum(Diabetes_Kidney_DALYs)
  ) %>%
  arrange(desc(Total_DALYs)) %>%
  slice(1:20)

top20_diabetes

# ==========================================================
# Top 20 Countries Bar Chart
# ==========================================================

ggplot(
  top20_diabetes,
  aes(
    x = reorder(Entity, Total_DALYs),
    y = Total_DALYs
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Countries by Diabetes & Kidney Disease Burden",
    x = "Country",
    y = "Total DALYs (1990–2019)"
  ) +
  theme_minimal()

country_data %>%
  filter(grepl("^OWID", Code)) %>%
  distinct(Entity, Code) %>%
  arrange(Entity)

# ==========================================================
# Global Diabetes & Kidney Disease Trend (1990–2019)
# ==========================================================

global_diabetes_trend <- country_data %>%
  group_by(Year) %>%
  summarise(
    Total_DALYs = sum(Diabetes_Kidney_DALYs),
    .groups = "drop"
  )

# View the trend table
global_diabetes_trend

# ==========================================================
# Trend Visualization
# ==========================================================

ggplot(global_diabetes_trend,
       aes(Year, Total_DALYs)) +
  
  geom_line(
    linewidth = 1.3,
    colour = "#0072B2"
  ) +
  
  geom_point(
    size = 2.5,
    colour = "#D55E00"
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    linetype = "dashed",
    colour = "red"
  ) +
  
  labs(
    title = "Global Diabetes & Kidney Disease Burden (1990–2019)",
    subtitle = "Trend across 205 countries",
    x = "Year",
    y = "Total DALYs"
  ) +
  
  theme_minimal(base_size = 13)
# ==========================================================
# Growth Analysis (1990–2019)
# ==========================================================

start_value <- global_diabetes_trend$Total_DALYs[1]
end_value   <- tail(global_diabetes_trend$Total_DALYs, 1)

absolute_growth <- end_value - start_value

percentage_growth <- ((end_value - start_value) / start_value) * 100

cagr <- ((end_value / start_value)^(1/29) - 1) * 100

growth_summary <- data.frame(
  Start_Year = 1990,
  End_Year = 2019,
  Start_DALYs = round(start_value),
  End_DALYs = round(end_value),
  Absolute_Growth = round(absolute_growth),
  Percentage_Growth = round(percentage_growth, 2),
  CAGR = round(cagr, 2)
)

growth_summary

# ==========================================================
# Top 10 Countries by Diabetes & Kidney Disease Burden (2019)
# ==========================================================

top10_2019 <- country_data %>%
  filter(Year == 2019) %>%
  arrange(desc(Diabetes_Kidney_DALYs)) %>%
  select(
    Entity,
    Diabetes_Kidney_DALYs
  ) %>%
  slice_head(n = 10)

top10_2019

# ==========================================================
# Top 10 Countries (2019) Visualization
# ==========================================================

ggplot(
  top10_2019,
  aes(
    x = reorder(Entity, Diabetes_Kidney_DALYs),
    y = Diabetes_Kidney_DALYs
  )
) +
  geom_col(fill = "#009E73") +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Diabetes & Kidney Disease Burden (2019)",
    subtitle = "Latest available year",
    x = "Country",
    y = "DALYs"
  ) +
  theme_minimal(base_size = 13)

# ==========================================================
# Countries with the Largest Increase (1990–2019)
# ==========================================================

country_growth <- country_data %>%
  filter(Year %in% c(1990, 2019)) %>%
  select(Entity, Year, Diabetes_Kidney_DALYs) %>%
  pivot_wider(
    names_from = Year,
    values_from = Diabetes_Kidney_DALYs
  ) %>%
  mutate(
    Absolute_Increase = `2019` - `1990`,
    Percentage_Increase = ((`2019` - `1990`) / `1990`) * 100
  ) %>%
  arrange(desc(Absolute_Increase))

head(country_growth, 10)

# ==========================================================
# Top 10 Countries by Absolute Increase
# ==========================================================

top_growth <- country_growth %>%
  slice_head(n = 10)

ggplot(
  top_growth,
  aes(
    x = reorder(Entity, Absolute_Increase),
    y = Absolute_Increase
  )
) +
  geom_col(fill = "#E69F00") +
  coord_flip() +
  labs(
    title = "Countries with the Largest Increase in Diabetes & Kidney Disease Burden",
    subtitle = "1990–2019",
    x = "Country",
    y = "Increase in DALYs"
  ) +
  theme_minimal(base_size = 13)

# ==========================================================
# Top 10 Countries by Percentage Increase
# ==========================================================

top_percentage_growth <- country_growth %>%
  arrange(desc(Percentage_Increase)) %>%
  slice_head(n = 10)

top_percentage_growth

ggplot(
  top_percentage_growth,
  aes(
    x = reorder(Entity, Percentage_Increase),
    y = Percentage_Increase
  )
) +
  geom_col(fill = "#CC79A7") +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Percentage Increase",
    subtitle = "Diabetes & Kidney Disease Burden (1990–2019)",
    x = "Country",
    y = "Percentage Increase (%)"
  ) +
  theme_minimal(base_size = 13)

# ==========================================================
# Available Regional Entities
# ==========================================================

regional_data %>%
  distinct(Entity) %>%
  arrange(Entity)

# ==========================================================
# WHO Regional Dataset
# ==========================================================

who_regions <- regional_data %>%
  filter(grepl("\\(WHO\\)", Entity))

who_regions %>%
  distinct(Entity) %>%
  arrange(Entity)

# ==========================================================
# Total Diabetes Burden by WHO Region
# ==========================================================

who_summary <- who_regions %>%
  group_by(Entity) %>%
  summarise(
    Total_DALYs = sum(Diabetes_Kidney_DALYs),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_DALYs))

who_summary

# ==========================================================
# WHO Regional Comparison
# ==========================================================

ggplot(
  who_summary,
  aes(
    x = reorder(Entity, Total_DALYs),
    y = Total_DALYs
  )
) +
  geom_col(fill = "#56B4E9") +
  coord_flip() +
  labs(
    title = "Diabetes & Kidney Disease Burden by WHO Region",
    subtitle = "1990–2019",
    x = "WHO Region",
    y = "Total DALYs"
  ) +
  theme_minimal(base_size = 13)

# ==========================================================
# WHO Regional Trends (1990–2019)
# ==========================================================

who_trend <- who_regions %>%
  group_by(Entity, Year) %>%
  summarise(
    Total_DALYs = sum(Diabetes_Kidney_DALYs),
    .groups = "drop"
  )

head(who_trend)

# ==========================================================
# WHO Regional Trend Plot
# ==========================================================

ggplot(
  who_trend,
  aes(
    x = Year,
    y = Total_DALYs,
    colour = Entity
  )
) +
  geom_line(linewidth = 1.1) +
  labs(
    title = "Diabetes & Kidney Disease Burden by WHO Region",
    subtitle = "1990–2019",
    x = "Year",
    y = "Total DALYs",
    colour = "WHO Region"
  ) +
  theme_minimal(base_size = 13)

# ==========================================================
# Correlation Analysis
# ==========================================================

disease_data <- country_data %>%
  select(
    Cirrhosis_Liver_DALYs,
    Mental_Disorders_DALYs,
    Chronic_Respiratory_DALYs,
    Neurological_DALYs,
    Cardiovascular_DALYs,
    Skin_DALYs,
    Substance_Use_DALYs,
    Musculoskeletal_DALYs,
    Neoplasms_DALYs,
    Digestive_DALYs,
    Other_NCDs_DALYs,
    Diabetes_Kidney_DALYs
  )

# ==========================================================
# Pearson Correlation Matrix
# ==========================================================

correlation_matrix <- cor(
  disease_data,
  method = "pearson"
)

round(correlation_matrix, 2)

# Install once
if (!"corrplot" %in% installed.packages()[, "Package"]) {
  install.packages("corrplot")
}

library(corrplot)

# ==========================================================
# Correlation Heatmap
# ==========================================================

corrplot(
  correlation_matrix,
  method = "color",
  type = "upper",
  order = "hclust",
  addCoef.col = "black",
  tl.col = "black",
  tl.cex = 0.8,
  number.cex = 0.7
)