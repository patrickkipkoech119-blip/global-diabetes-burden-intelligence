library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(leaflet)
library(rnaturalearth)
library(sf)
library(forecast)
library(DT)
library(countrycode)
library(scales)

# ==============================
# LOAD DATA
# ==============================

data <- read_csv("data/diabetes_clean.csv", show_col_types = FALSE) %>%
  mutate(
    year = as.numeric(year),
    daly = as.numeric(daly),
    iso3 = countrycode(country, "country.name", "iso3c"),
    continent = countrycode(country, "country.name", "continent")
  ) %>%
  filter(!is.na(iso3), !is.na(daly))

years_available <- sort(unique(data$year))

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(iso3 = iso_a3)

# ==============================
# UI
# ==============================

ui <- dashboardPage(
  
  dashboardHeader(
    titleWidth = 450,
    title = "Global Burden of Diabetes & Kidney Diseases"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Executive Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Geographic Intelligence", tabName = "map", icon = icon("globe")),
      menuItem("Country Benchmarking", tabName = "benchmark", icon = icon("flag")),
      menuItem("Continent Comparison", tabName = "continent", icon = icon("earth")),
      menuItem("Country Rankings", tabName = "ranking", icon = icon("list-ol")),
      menuItem("Inequality Analysis", tabName = "inequality", icon = icon("chart-bar")),
      menuItem("Forecast & Projection", tabName = "forecast", icon = icon("chart-area"))
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }
        h2 { font-weight: 600; }
      "))
    ),
    
    tabItems(
      
      # ==============================
      # EXECUTIVE OVERVIEW
      # ==============================
      
      tabItem(tabName = "overview",
              
              fluidRow(
                box(
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  title = "Project Overview",
                  p("This dashboard presents a comprehensive spatiotemporal analysis of the global burden of diabetes and kidney diseases from 1990 to 2019."),
                  p("It integrates geographic intelligence, inequality measurement, country benchmarking, and ARIMA-based forecasting to support public health insight and policy evaluation.")
                )
              ),
              
              fluidRow(
                valueBoxOutput("total_countries"),
                valueBoxOutput("latest_global"),
                valueBoxOutput("growth_metric")
              ),
              
              fluidRow(
                box(width = 12, title = "Global Trend (1990–2019)",
                    plotlyOutput("global_trend"))
              )
      ),
      
      # ==============================
      # MAP
      # ==============================
      
      tabItem(tabName = "map",
              sliderInput("map_year", "Select Year:",
                          min = min(years_available),
                          max = max(years_available),
                          value = max(years_available),
                          sep = ""),
              leafletOutput("worldMap", height = 600)
      ),
      
      # ==============================
      # BENCHMARK
      # ==============================
      
      tabItem(tabName = "benchmark",
              selectInput("country_select", "Select Country:",
                          choices = sort(unique(data$country))),
              plotlyOutput("country_trend")
      ),
      
      # ==============================
      # CONTINENT
      # ==============================
      
      tabItem(tabName = "continent",
              plotlyOutput("continent_plot")
      ),
      
      # ==============================
      # RANKING
      # ==============================
      
      tabItem(tabName = "ranking",
              sliderInput("rank_year", "Select Year:",
                          min = min(years_available),
                          max = max(years_available),
                          value = max(years_available),
                          sep = ""),
              DTOutput("ranking_table")
      ),
      
      # ==============================
      # INEQUALITY
      # ==============================
      
      tabItem(tabName = "inequality",
              plotOutput("gini_plot", height = 400)
      ),
      
      # ==============================
      # FORECAST
      # ==============================
      
      tabItem(tabName = "forecast",
              plotOutput("forecast_plot", height = 500)
      )
    )
  )
)

# ==============================
# SERVER
# ==============================

server <- function(input, output, session) {
  
  output$total_countries <- renderValueBox({
    valueBox(
      comma(n_distinct(data$country)),
      "Total Countries",
      icon = icon("globe"),
      color = "blue"
    )
  })
  
  output$latest_global <- renderValueBox({
    latest <- data %>%
      filter(year == max(year)) %>%
      summarise(mean_val = mean(daly, na.rm = TRUE), .groups = "drop")
    
    valueBox(
      comma(round(latest$mean_val, 0)),
      "Latest Global Avg DALY",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$growth_metric <- renderValueBox({
    growth <- data %>%
      group_by(year) %>%
      summarise(mean_daly = mean(daly, na.rm = TRUE), .groups = "drop") %>%
      arrange(year)
    
    total_growth <- last(growth$mean_daly) - first(growth$mean_daly)
    
    valueBox(
      comma(round(total_growth, 0)),
      "Total Growth (1990 → 2019)",
      icon = icon("arrow-up"),
      color = "red"
    )
  })
  
  output$global_trend <- renderPlotly({
    global <- data %>%
      group_by(year) %>%
      summarise(mean_daly = mean(daly, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(global, aes(year, mean_daly)) +
      geom_line(linewidth = 1.3, color = "#2C3E50") +
      labs(x = "Year", y = "Average DALYs") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$worldMap <- renderLeaflet({
    map_data <- world %>%
      left_join(data %>% filter(year == input$map_year),
                by = "iso3")
    
    pal <- colorNumeric("viridis",
                        domain = map_data$daly,
                        na.color = "lightgrey")
    
    leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(daly),
        fillOpacity = 0.8,
        weight = 0.5,
        color = "white",
        label = ~paste0(name, ": ", comma(round(daly, 0)))
      ) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~daly,
                title = "DALY")
  })
  
  output$country_trend <- renderPlotly({
    df <- data %>% filter(country == input$country_select)
    
    p <- ggplot(df, aes(year, daly)) +
      geom_line(linewidth = 1.2, color = "#8E44AD") +
      labs(x = "Year", y = "DALYs") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$continent_plot <- renderPlotly({
    cont <- data %>%
      group_by(continent, year) %>%
      summarise(mean_daly = mean(daly, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(cont, aes(year, mean_daly, color = continent)) +
      geom_line(linewidth = 1) +
      labs(x = "Year", y = "Average DALYs") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$ranking_table <- renderDT({
    data %>%
      filter(year == input$rank_year) %>%
      arrange(desc(daly)) %>%
      select(country, daly) %>%
      mutate(daly = comma(round(daly, 0))) %>%
      datatable(options = list(pageLength = 10))
  })
  
  gini_manual <- function(x){
    x <- sort(x)
    n <- length(x)
    sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
  }
  
  output$gini_plot <- renderPlot({
    gini_df <- data %>%
      group_by(year) %>%
      summarise(gini = gini_manual(daly), .groups = "drop")
    
    ggplot(gini_df, aes(year, gini)) +
      geom_line(linewidth = 1.2, color = "#C0392B") +
      labs(x = "Year", y = "Global Gini Coefficient") +
      theme_minimal()
  })
  
  output$forecast_plot <- renderPlot({
    global <- data %>%
      group_by(year) %>%
      summarise(mean_daly = mean(daly, na.rm = TRUE), .groups = "drop") %>%
      arrange(year)
    
    ts_data <- ts(global$mean_daly,
                  start = min(global$year),
                  frequency = 1)
    
    model <- auto.arima(ts_data)
    fc <- forecast(model, h = 5)
    
    plot(fc,
         main = "ARIMA Forecast of Global Average DALYs",
         ylab = "DALYs",
         xlab = "Year")
  })
}

shinyApp(ui, server)