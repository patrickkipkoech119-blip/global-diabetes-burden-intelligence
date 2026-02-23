# ==============================================================
# STEP 2: EXPLORATORY DATA ANALYSIS (EDA)
# Project: Global Trends in Diabetes & Kidney Disease Burden
# ==============================================================

library(tidyverse)
library(ggplot2)
library(scales)

# --------------------------------------------------------------
# 1️⃣ Load Clean Dataset
# --------------------------------------------------------------

data <- read_csv("data/diabetes_clean.csv", show_col_types = FALSE)

# --------------------------------------------------------------
# 2️⃣ Basic Structure & Summary
# --------------------------------------------------------------

cat("----- DATA OVERVIEW -----\n")
cat("Rows:", nrow(data), "\n")
cat("Countries:", n_distinct(data$country), "\n")
cat("Year Range:", min(data$year), "-", max(data$year), "\n\n")

summary(data$daly)

# --------------------------------------------------------------
# 3️⃣ Global Trend Over Time
# --------------------------------------------------------------

global_trend <- data %>%
  group_by(year) %>%
  summarise(mean_daly = mean(daly, na.rm = TRUE),
            .groups = "drop")

ggplot(global_trend, aes(x = year, y = mean_daly)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  labs(
    title = "Global Average DALYs Trend",
    x = "Year",
    y = "Average DALYs"
  ) +
  theme_minimal()

# --------------------------------------------------------------
# 4️⃣ Top 10 Countries (Latest Year)
# --------------------------------------------------------------

latest_year <- max(data$year)

top10 <- data %>%
  filter(year == latest_year) %>%
  arrange(desc(daly)) %>%
  slice_head(n = 10)

ggplot(top10, aes(x = reorder(country, daly), y = daly)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = paste("Top 10 Countries by DALYs (", latest_year, ")", sep=""),
    x = "Country",
    y = "DALYs"
  ) +
  theme_minimal()

# --------------------------------------------------------------
# 5️⃣ Distribution of DALYs
# --------------------------------------------------------------

ggplot(data, aes(x = daly)) +
  geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7) +
  labs(
    title = "Distribution of DALYs Across Countries",
    x = "DALYs",
    y = "Frequency"
  ) +
  theme_minimal()

# --------------------------------------------------------------
# 6️⃣ Country-Level Trend Example (Top Country)
# --------------------------------------------------------------

top_country <- top10$country[1]

country_trend <- data %>%
  filter(country == top_country)

ggplot(country_trend, aes(x = year, y = daly)) +
  geom_line(linewidth = 1.2, color = "purple") +
  labs(
    title = paste("Trend for", top_country),
    x = "Year",
    y = "DALYs"
  ) +
  theme_minimal()

cat("✅ STEP 2 COMPLETE: EDA Visualizations Generated Successfully\n")