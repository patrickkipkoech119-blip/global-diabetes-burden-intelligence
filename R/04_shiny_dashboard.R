############################################################
# Project : Global NCD Intelligence Dashboard
# File    : 04_shiny_dashboard.R
# Author  : KIPKOECH PATRICK
# Course  : SDS 6103 - Statistical Computing
# Program : MSc Data Science
############################################################

# ==========================================================
# Install Required Packages (Run Once)
# ==========================================================

required_packages <- c(
  "shiny", "shinydashboard", "plotly", "DT", "leaflet",
  "dplyr", "readr", "tidyr", "ggplot2", "scales",
  "sf", "rnaturalearth", "countrycode", "forecast"
)

new_packages <- required_packages[
  !(required_packages %in% installed.packages()[, "Package"])
]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

# ==========================================================
# Load Packages
# ==========================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(leaflet)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)
library(sf)
library(rnaturalearth)
library(countrycode)
library(forecast)

# ==========================================================
# Load Base Data
# ==========================================================
# The app now works directly from the long-format country and
# regional datasets (rather than the pre-aggregated per-tab
# CSVs), so every tab can share the same filters and stay
# consistent with each other.

country_data_raw <- read_csv("../data/country_data.csv", show_col_types = FALSE)
regional_data_raw <- read_csv("../data/regional_data.csv", show_col_types = FALSE)

country_data_raw <- country_data_raw %>%
  filter(!grepl("^OWID", Code)) %>%
  mutate(
    Continent = countrycode(
      Code,
      origin = "iso3c",
      destination = "continent"
    )
  ) %>%
  filter(!is.na(Continent))

who_regions <- regional_data_raw %>%
  filter(grepl("\\(WHO\\)", Entity))

disease_cols <- c(
  Cirrhosis          = "Cirrhosis_Liver_DALYs",
  Mental             = "Mental_Disorders_DALYs",
  Respiratory        = "Chronic_Respiratory_DALYs",
  Neurological       = "Neurological_DALYs",
  Cardiovascular     = "Cardiovascular_DALYs",
  Skin               = "Skin_DALYs",
  Substance          = "Substance_Use_DALYs",
  Musculoskeletal    = "Musculoskeletal_DALYs",
  Neoplasms          = "Neoplasms_DALYs",
  Digestive          = "Digestive_DALYs",
  Other_NCDs         = "Other_NCDs_DALYs",
  Diabetes_Kidney    = "Diabetes_Kidney_DALYs"
)

YEAR_MIN <- min(country_data_raw$Year)
YEAR_MAX <- max(country_data_raw$Year)
CONTINENT_CHOICES <- sort(unique(country_data_raw$Continent))

# Small helper so every tab can get a download button without
# repeating the same six lines each time.
download_ui <- function(id, label = "Download this view (CSV)") {
  downloadButton(id, label, class = "btn-block")
}

download_server <- function(output, id, data_fn, filename) {
  output[[id]] <- downloadHandler(
    filename = function() filename,
    content = function(file) write.csv(data_fn(), file, row.names = FALSE)
  )
}

# ==========================================================
# User Interface
# ==========================================================

ui <- dashboardPage(
  
  dashboardHeader(title = "Global NCD Intelligence"),
  
  dashboardSidebar(
    
    sidebarMenu(
      id = "tabs",
      menuItem("About & Data Notes", tabName = "about", icon = icon("circle-info")),
      menuItem("Executive Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Global Disease Overview", tabName = "disease", icon = icon("globe")),
      menuItem("Geographic Intelligence", tabName = "map", icon = icon("map")),
      menuItem("Country Rankings", tabName = "ranking", icon = icon("trophy")),
      menuItem("Global Trends", tabName = "trend", icon = icon("chart-line")),
      menuItem("Continent Analysis", tabName = "continent", icon = icon("globe")),
      menuItem("WHO Regions", tabName = "who", icon = icon("hospital")),
      menuItem("Forecast & Projection", tabName = "forecast", icon = icon("chart-area")),
      menuItem("Data Explorer", tabName = "table", icon = icon("table"))
    ),
    
    hr(),
    
    div(
      style = "padding: 0 15px 15px 15px; color: #b8c7ce;",
      
      strong("Global filters"),
      p(style = "font-size: 12px; margin-top: 4px;",
        "Applies to Overview, Disease Overview, Map, Rankings, Trends and Continent tabs."),
      
      sliderInput(
        "year_range", "Year range",
        min = YEAR_MIN, max = YEAR_MAX,
        value = c(YEAR_MIN, YEAR_MAX),
        sep = "", step = 1
      ),
      
      selectizeInput(
        "continent_filter_global", "Continent(s)",
        choices = CONTINENT_CHOICES,
        selected = CONTINENT_CHOICES,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      #==========================================================
      # About & Data Notes
      #==========================================================
      tabItem(
        tabName = "about",
        
        fluidRow(
          box(
            title = "Who this dashboard is for", status = "primary",
            solidHeader = TRUE, width = 12,
            
            p("This dashboard is built to support three types of decisions:"),
            tags$ul(
              tags$li(strong("Surveillance:"), " tracking how the global and regional burden of diabetes and kidney disease has changed, 1990-2019."),
              tags$li(strong("Resource targeting:"), " identifying which countries carry the highest burden relative to population/continent, to inform where attention is most needed."),
              tags$li(strong("Planning:"), " using the short-term forecast to anticipate the near-term trajectory, with its uncertainty made explicit.")
            ),
            p("It is a descriptive and comparative intelligence tool. It does not perform causal analysis -- differences between countries reflect many factors (data quality, health system capacity, reporting practices, population age structure) that this dashboard does not model.")
          )
        ),
        
        fluidRow(
          box(
            title = "Data note: Diabetes and Kidney diseases are combined at source",
            status = "warning", solidHeader = TRUE, width = 12,
            
            p("The Global Burden of Disease (GBD) study, which underlies this data, groups diabetes mellitus and chronic kidney disease into a single Level 2 cause category, ",
              strong("\"Diabetes and kidney diseases,\""),
              " with the two conditions broken out as separate Level 3 causes underneath it."),
            p("Our World in Data's public export -- the source used here -- only exposes the combined Level 2 total, not the Level 3 breakdown. This dashboard therefore reports a ",
              strong("combined"), " diabetes-and-kidney-disease burden throughout."),
            p("This matters because diabetes and CKD have different risk factors and different prevention strategies, and not all CKD is diabetes-related. A country ranked high on the combined measure could be driven by either condition, or both -- this dashboard cannot distinguish which. Separating them would require the GBD Results Tool directly (", tags$code("vizhub.healthdata.org/gbd-results"), "), querying \"Diabetes mellitus\" and \"Chronic kidney disease\" as separate causes -- noted here as a direction for future work rather than left undisclosed.")
          )
        )
      ),
      
      #==========================================================
      # Executive Overview
      #==========================================================
      tabItem(
        tabName = "overview",
        
        fluidRow(
          valueBoxOutput("countries", width = 3),
          valueBoxOutput("years", width = 3),
          valueBoxOutput("total", width = 3),
          valueBoxOutput("maximum", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Global Diabetes & Kidney Disease Trend (filtered)",
            status = "primary", solidHeader = TRUE, width = 12,
            plotlyOutput("trend_plot", height = 350)
          )
        ),
        
        fluidRow(
          box(
            title = "Global Disease Burden Distribution (filtered)",
            status = "success", solidHeader = TRUE, width = 6,
            plotlyOutput("disease_plot", height = 350)
          ),
          box(
            title = "Top 10 Countries (latest year in range)",
            status = "warning", solidHeader = TRUE, width = 6,
            DTOutput("top10_table"),
            br(),
            download_ui("download_top10")
          )
        )
      ),
      
      #==========================================================
      # Global Disease Overview
      #==========================================================
      tabItem(
        tabName = "disease",
        
        fluidRow(
          box(
            title = "Global Disease Burden Distribution (filtered)",
            width = 12, status = "primary", solidHeader = TRUE,
            plotlyOutput("disease_bar", height = 550)
          )
        ),
        
        fluidRow(
          box(
            title = "Disease Burden Dataset (filtered)",
            width = 12, status = "success", solidHeader = TRUE,
            DTOutput("disease_table"),
            br(),
            download_ui("download_disease")
          )
        )
      ),
      
      #==========================================================
      # Geographic Intelligence
      #==========================================================
      tabItem(
        tabName = "map",
        
        fluidRow(
          box(
            title = "Diabetes & Kidney Disease Burden Map (latest year in range, filtered continents)",
            width = 12, status = "primary", solidHeader = TRUE,
            leafletOutput("world_map", height = "700px"),
            br(),
            download_ui("download_map")
          )
        )
      ),
      
      #==========================================================
      # Country Rankings
      #==========================================================
      tabItem(
        tabName = "ranking",
        
        fluidRow(
          box(
            title = "Ranking Filters", status = "warning",
            solidHeader = TRUE, width = 3,
            
            helpText("Continent and year range use the global filters on the left. These add further narrowing:"),
            
            selectInput("top_n", "Show", choices = c(10, 20, 50, "All"), selected = 20),
            
            textInput("country_search", "Search Country", placeholder = "Type country name..."),
            
            download_ui("download_rankings")
          ),
          
          box(
            title = "Country Rankings (filtered)",
            status = "primary", solidHeader = TRUE, width = 9,
            plotlyOutput("ranking_plot", height = 550)
          )
        ),
        
        fluidRow(
          box(
            title = "Fastest-Growing Burden, first vs. last year in range",
            status = "info", solidHeader = TRUE, width = 12,
            plotlyOutput("growth_plot", height = 350)
          )
        ),
        
        fluidRow(
          box(
            title = "Ranking Table (filtered)", width = 12,
            status = "success", solidHeader = TRUE,
            DTOutput("ranking_table")
          )
        )
      ),
      
      #==========================================================
      # Global Trends
      #==========================================================
      tabItem(
        tabName = "trend",
        
        fluidRow(
          box(
            title = "Global Diabetes & Kidney Disease Trend (filtered)",
            status = "primary", solidHeader = TRUE, width = 12,
            plotlyOutput("global_trend_plot", height = 450)
          )
        ),
        
        fluidRow(
          box(
            title = "Trend Data (filtered)", status = "success",
            solidHeader = TRUE, width = 12,
            DTOutput("global_trend_table"),
            br(),
            download_ui("download_trend")
          )
        )
      ),
      
      #==========================================================
      # Continent Analysis
      #==========================================================
      tabItem(
        tabName = "continent",
        
        fluidRow(
          box(
            title = "Continent Summary (filtered)", status = "primary",
            solidHeader = TRUE, width = 4,
            DTOutput("continent_summary_table"),
            br(),
            download_ui("download_continent_summary")
          ),
          box(
            title = "Total DALYs by Continent (filtered)", status = "success",
            solidHeader = TRUE, width = 8,
            plotlyOutput("continent_bar", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = "Continent Trends Over Time (filtered)", status = "warning",
            solidHeader = TRUE, width = 12,
            plotlyOutput("continent_trend_plot", height = 500)
          )
        )
      ),
      
      #==========================================================
      # WHO Regions
      #==========================================================
      tabItem(
        tabName = "who",
        
        fluidRow(
          box(
            title = "WHO Regional Summary (year range applied)", status = "primary",
            solidHeader = TRUE, width = 4,
            DTOutput("who_summary_table"),
            br(),
            download_ui("download_who_summary")
          ),
          box(
            title = "WHO Regional Burden", status = "success",
            solidHeader = TRUE, width = 8,
            plotlyOutput("who_bar", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = "WHO Regional Trends", status = "warning",
            solidHeader = TRUE, width = 12,
            plotlyOutput("who_trend_plot", height = 500)
          )
        )
      ),
      
      #==========================================================
      # Forecast & Projection
      #==========================================================
      tabItem(
        tabName = "forecast",
        
        fluidRow(
          box(
            title = "Forecast scope", status = "warning", solidHeader = TRUE, width = 3,
            
            radioButtons("forecast_scope", "Forecast for:",
                         choices = c("Global (all countries)" = "global", "A specific country" = "country"),
                         selected = "global"),
            
            conditionalPanel(
              condition = "input.forecast_scope == 'country'",
              selectizeInput("forecast_country", "Country",
                             choices = sort(unique(country_data_raw$Entity)),
                             selected = "Kenya")
            ),
            
            helpText(paste0("Uses the full ", YEAR_MIN, "-", YEAR_MAX,
                            " history regardless of the year-range filter, since ARIMA needs the complete series to be statistically valid.")),
            
            download_ui("download_forecast", "Download forecast table (CSV)")
          ),
          
          box(
            title = "5-Year ARIMA Forecast, with 80% and 95% prediction intervals",
            status = "primary", solidHeader = TRUE, width = 9,
            plotlyOutput("forecast_plot", height = 450)
          )
        ),
        
        fluidRow(
          box(
            title = "Forecast values and prediction intervals",
            status = "success", solidHeader = TRUE, width = 12,
            p("Point forecasts are estimates, not certainties. The wider the interval, the less confidence the model has in that year's number -- intervals widen the further out the forecast goes."),
            DTOutput("forecast_table")
          )
        )
      ),
      
      #==========================================================
      # Data Explorer
      #==========================================================
      tabItem(
        tabName = "table",
        
        fluidRow(
          box(
            title = "Select Dataset", status = "primary", solidHeader = TRUE, width = 3,
            
            selectInput(
              "dataset_choice", "Dataset",
              choices = c(
                "Country-level data (filtered)",
                "Country Rankings (filtered)",
                "Continent Summary (filtered)",
                "WHO Summary (year range)",
                "Global Disease Summary (filtered)"
              )
            ),
            
            download_ui("download_data", "Download Selected Dataset")
          ),
          
          box(
            title = "Dataset Preview", status = "success", solidHeader = TRUE, width = 9,
            DTOutput("data_explorer")
          )
        )
      )
    )
  )
)

# ==========================================================
# Server
# ==========================================================

server <- function(input, output, session) {
  
  #----------------------------------------------------------
  # Shared filtered country-level data (drives most tabs)
  #----------------------------------------------------------
  
  filtered_country <- reactive({
    req(input$year_range, input$continent_filter_global)
    
    country_data_raw %>%
      filter(
        Year >= input$year_range[1],
        Year <= input$year_range[2],
        Continent %in% input$continent_filter_global
      )
  })
  
  filtered_who <- reactive({
    req(input$year_range)
    who_regions %>%
      filter(Year >= input$year_range[1], Year <= input$year_range[2])
  })
  
  latest_year_in_range <- reactive({
    max(filtered_country()$Year)
  })
  
  #----------------------------------------------------------
  # Executive Overview
  #----------------------------------------------------------
  
  output$countries <- renderValueBox({
    valueBox(n_distinct(filtered_country()$Entity), "Countries", icon = icon("globe"), color = "blue")
  })
  
  output$years <- renderValueBox({
    valueBox(n_distinct(filtered_country()$Year), "Years", icon = icon("calendar"), color = "green")
  })
  
  output$total <- renderValueBox({
    valueBox(comma(round(sum(filtered_country()$Diabetes_Kidney_DALYs))),
             "Total Diabetes & Kidney DALYs (filtered)", icon = icon("heartbeat"), color = "red")
  })
  
  output$maximum <- renderValueBox({
    valueBox(comma(round(max(filtered_country()$Diabetes_Kidney_DALYs))),
             "Maximum single country-year DALYs", icon = icon("chart-line"), color = "yellow")
  })
  
  global_trend_data <- reactive({
    filtered_country() %>%
      group_by(Year) %>%
      summarise(Total_DALYs = sum(Diabetes_Kidney_DALYs), .groups = "drop")
  })
  
  output$trend_plot <- renderPlotly({
    p <- ggplot(global_trend_data(), aes(x = Year, y = Total_DALYs)) +
      geom_line(colour = "steelblue", linewidth = 1.2) +
      geom_point() +
      labs(x = "Year", y = "Total DALYs") +
      theme_minimal()
    ggplotly(p)
  })
  
  disease_summary_data <- reactive({
    df <- filtered_country()
    tibble(Disease = names(disease_cols)) %>%
      rowwise() %>%
      mutate(Total_DALYs = sum(df[[disease_cols[Disease]]])) %>%
      ungroup() %>%
      mutate(Percentage = round(Total_DALYs / sum(Total_DALYs) * 100, 2)) %>%
      arrange(desc(Total_DALYs))
  })
  
  output$disease_plot <- renderPlotly({
    p <- ggplot(disease_summary_data(), aes(x = reorder(Disease, Total_DALYs), y = Total_DALYs)) +
      geom_col(fill = "darkgreen") + coord_flip() +
      labs(x = "Disease", y = "Total DALYs") + theme_minimal()
    ggplotly(p)
  })
  
  top10_data <- reactive({
    filtered_country() %>%
      filter(Year == latest_year_in_range()) %>%
      arrange(desc(Diabetes_Kidney_DALYs)) %>%
      select(Entity, Code, Continent, Year, Diabetes_Kidney_DALYs) %>%
      slice_head(n = 10)
  })
  
  output$top10_table <- renderDT({
    datatable(top10_data(), options = list(pageLength = 10), rownames = FALSE)
  })
  
  download_server(output, "download_top10", top10_data, "top10_countries.csv")
  
  #----------------------------------------------------------
  # Global Disease Overview
  #----------------------------------------------------------
  
  output$disease_bar <- renderPlotly({
    p <- ggplot(disease_summary_data(), aes(x = reorder(Disease, Total_DALYs), y = Total_DALYs, fill = Disease)) +
      geom_col(show.legend = FALSE) + coord_flip() +
      labs(x = "Disease Group", y = "Total DALYs") + theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$disease_table <- renderDT({
    datatable(disease_summary_data(), options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })
  
  download_server(output, "download_disease", disease_summary_data, "disease_summary_filtered.csv")
  
  #----------------------------------------------------------
  # Geographic Intelligence - World Map
  #----------------------------------------------------------
  
  map_data_reactive <- reactive({
    filtered_country() %>%
      filter(Year == latest_year_in_range()) %>%
      select(Entity, Code, Continent, Year, Diabetes_Kidney_DALYs)
  })
  
  output$world_map <- renderLeaflet({
    world <- ne_countries(scale = "medium", returnclass = "sf")
    world <- world %>% left_join(map_data_reactive(), by = c("iso_a3" = "Code"))
    
    pal <- colorNumeric(palette = "YlOrRd", domain = world$Diabetes_Kidney_DALYs, na.color = "#DDDDDD")
    
    leaflet(world) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(lng1 = -180, lat1 = -60, lng2 = 180, lat2 = 85) %>%
      addPolygons(
        fillColor = ~pal(Diabetes_Kidney_DALYs), fillOpacity = 0.8,
        color = "white", weight = 0.5, smoothFactor = 0.2,
        popup = ~paste0("<b>", name, "</b><br>",
                        "<b>Continent:</b> ", continent, "<br>",
                        "<b>Diabetes & Kidney DALYs:</b> ", comma(round(Diabetes_Kidney_DALYs))),
        highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 1, bringToFront = TRUE)
      ) %>%
      addLegend(position = "bottomright", pal = pal, values = ~Diabetes_Kidney_DALYs,
                title = "Diabetes & Kidney DALYs", opacity = 0.8)
  })
  
  download_server(output, "download_map", map_data_reactive, "map_data_filtered.csv")
  
  #----------------------------------------------------------
  # Country Rankings
  #----------------------------------------------------------
  
  country_rankings_data <- reactive({
    filtered_country() %>%
      group_by(Entity, Code, Continent) %>%
      summarise(Total_DALYs = sum(Diabetes_Kidney_DALYs),
                Mean_DALYs = mean(Diabetes_Kidney_DALYs), .groups = "drop") %>%
      arrange(desc(Total_DALYs))
  })
  
  ranking_data <- reactive({
    df <- country_rankings_data()
    
    if (input$country_search != "") {
      df <- df %>% filter(grepl(input$country_search, Entity, ignore.case = TRUE))
    }
    
    if (input$top_n != "All") {
      df <- head(df, as.numeric(input$top_n))
    }
    
    df
  })
  
  output$ranking_plot <- renderPlotly({
    p <- ggplot(ranking_data(), aes(x = reorder(Entity, Total_DALYs), y = Total_DALYs)) +
      geom_col(fill = "steelblue") + coord_flip() +
      labs(x = "Country", y = "Total DALYs") + theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$ranking_table <- renderDT({
    datatable(ranking_data(), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  download_server(output, "download_rankings", ranking_data, "country_rankings_filtered.csv")
  
  growth_data <- reactive({
    yr <- input$year_range
    filtered_country() %>%
      filter(Year %in% c(yr[1], yr[2])) %>%
      select(Entity, Code, Continent, Year, Diabetes_Kidney_DALYs) %>%
      pivot_wider(names_from = Year, values_from = Diabetes_Kidney_DALYs, names_prefix = "Y") %>%
      mutate(
        Absolute_Increase = .data[[paste0("Y", yr[2])]] - .data[[paste0("Y", yr[1])]],
        Percentage_Increase = Absolute_Increase / .data[[paste0("Y", yr[1])]] * 100
      ) %>%
      arrange(desc(Absolute_Increase)) %>%
      slice_head(n = 15)
  })
  
  output$growth_plot <- renderPlotly({
    p <- ggplot(growth_data(), aes(x = reorder(Entity, Absolute_Increase), y = Absolute_Increase)) +
      geom_col(fill = "#C0392B") + coord_flip() +
      labs(x = "Country", y = paste0("Increase in DALYs, ", input$year_range[1], " to ", input$year_range[2])) +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })
  
  #----------------------------------------------------------
  # Global Trends tab (same series as Overview, standalone view)
  #----------------------------------------------------------
  
  output$global_trend_plot <- renderPlotly({
    p <- ggplot(global_trend_data(), aes(x = Year, y = Total_DALYs)) +
      geom_line(colour = "steelblue", linewidth = 1.2) + geom_point() +
      labs(x = "Year", y = "Total DALYs") + theme_minimal()
    ggplotly(p)
  })
  
  output$global_trend_table <- renderDT({
    datatable(global_trend_data(), options = list(pageLength = 15), rownames = FALSE)
  })
  
  download_server(output, "download_trend", global_trend_data, "global_trend_filtered.csv")
  
  #----------------------------------------------------------
  # Continent Analysis
  #----------------------------------------------------------
  
  continent_summary_data <- reactive({
    filtered_country() %>%
      group_by(Continent) %>%
      summarise(Countries = n_distinct(Entity), Total_DALYs = sum(Diabetes_Kidney_DALYs),
                Mean_DALYs = mean(Diabetes_Kidney_DALYs), Median_DALYs = median(Diabetes_Kidney_DALYs),
                .groups = "drop") %>%
      arrange(desc(Total_DALYs))
  })
  
  continent_trend_data <- reactive({
    filtered_country() %>%
      group_by(Continent, Year) %>%
      summarise(Total_DALYs = sum(Diabetes_Kidney_DALYs), .groups = "drop")
  })
  
  output$continent_summary_table <- renderDT({
    datatable(continent_summary_data(), options = list(pageLength = 10), rownames = FALSE)
  })
  
  download_server(output, "download_continent_summary", continent_summary_data, "continent_summary_filtered.csv")
  
  output$continent_bar <- renderPlotly({
    p <- ggplot(continent_summary_data(), aes(x = reorder(Continent, Total_DALYs), y = Total_DALYs)) +
      geom_col(fill = "darkgreen") + coord_flip() +
      labs(x = "Continent", y = "Total DALYs") + theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$continent_trend_plot <- renderPlotly({
    p <- ggplot(continent_trend_data(), aes(x = Year, y = Total_DALYs, colour = Continent)) +
      geom_line(linewidth = 1) +
      labs(x = "Year", y = "Total DALYs") + theme_minimal()
    ggplotly(p)
  })
  
  #----------------------------------------------------------
  # WHO Regions (year range only -- these are pre-aggregated
  # OWID regional rows, not individual countries, so the
  # continent filter does not apply)
  #----------------------------------------------------------
  
  who_summary_data <- reactive({
    filtered_who() %>%
      group_by(Entity) %>%
      summarise(Total_DALYs = sum(Diabetes_Kidney_DALYs), Mean_DALYs = mean(Diabetes_Kidney_DALYs), .groups = "drop") %>%
      arrange(desc(Total_DALYs))
  })
  
  who_trend_data <- reactive({
    filtered_who() %>%
      group_by(Entity, Year) %>%
      summarise(Total_DALYs = sum(Diabetes_Kidney_DALYs), .groups = "drop")
  })
  
  output$who_summary_table <- renderDT({
    datatable(who_summary_data(), options = list(pageLength = 10), rownames = FALSE)
  })
  
  download_server(output, "download_who_summary", who_summary_data, "who_summary_filtered.csv")
  
  output$who_bar <- renderPlotly({
    p <- ggplot(who_summary_data(), aes(x = reorder(Entity, Total_DALYs), y = Total_DALYs)) +
      geom_col(fill = "steelblue") + coord_flip() +
      labs(x = "WHO Region", y = "Total DALYs") + theme_minimal(base_size = 13)
    ggplotly(p)
  })
  
  output$who_trend_plot <- renderPlotly({
    p <- ggplot(who_trend_data(), aes(x = Year, y = Total_DALYs, colour = Entity)) +
      geom_line(linewidth = 1) +
      labs(x = "Year", y = "Total DALYs", colour = "WHO Region") + theme_minimal()
    ggplotly(p)
  })
  
  #----------------------------------------------------------
  # Forecast & Projection
  #----------------------------------------------------------
  
  forecast_series <- reactive({
    if (input$forecast_scope == "global") {
      country_data_raw %>%
        group_by(Year) %>%
        summarise(Total_DALYs = sum(Diabetes_Kidney_DALYs), .groups = "drop") %>%
        arrange(Year)
    } else {
      req(input$forecast_country)
      country_data_raw %>%
        filter(Entity == input$forecast_country) %>%
        select(Year, Total_DALYs = Diabetes_Kidney_DALYs) %>%
        arrange(Year)
    }
  })
  
  forecast_result <- reactive({
    hist <- forecast_series()
    ts_data <- ts(hist$Total_DALYs, start = min(hist$Year), frequency = 1)
    model <- auto.arima(ts_data)
    forecast(model, h = 5, level = c(80, 95))
  })
  
  forecast_table_data <- reactive({
    fc <- forecast_result()
    hist <- forecast_series()
    last_year <- max(hist$Year)
    
    tibble(
      Year = (last_year + 1):(last_year + 5),
      Point_Forecast = as.numeric(fc$mean),
      Lo_80 = as.numeric(fc$lower[, 1]),
      Hi_80 = as.numeric(fc$upper[, 1]),
      Lo_95 = as.numeric(fc$lower[, 2]),
      Hi_95 = as.numeric(fc$upper[, 2])
    )
  })
  
  output$forecast_plot <- renderPlotly({
    hist <- forecast_series()
    ft <- forecast_table_data()
    
    p <- ggplot() +
      geom_line(data = hist, aes(x = Year, y = Total_DALYs), colour = "steelblue", linewidth = 1) +
      geom_ribbon(data = ft, aes(x = Year, ymin = Lo_95, ymax = Hi_95), fill = "orange", alpha = 0.2) +
      geom_ribbon(data = ft, aes(x = Year, ymin = Lo_80, ymax = Hi_80), fill = "orange", alpha = 0.35) +
      geom_line(data = ft, aes(x = Year, y = Point_Forecast), colour = "#C0392B", linewidth = 1, linetype = "dashed") +
      geom_point(data = ft, aes(x = Year, y = Point_Forecast), colour = "#C0392B") +
      labs(x = "Year", y = "Total DALYs",
           title = paste0("Forecast: ", ifelse(input$forecast_scope == "global", "Global", input$forecast_country))) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$forecast_table <- renderDT({
    datatable(forecast_table_data() %>%
                mutate(across(-Year, ~ comma(round(.x)))),
              options = list(pageLength = 5), rownames = FALSE)
  })
  
  download_server(output, "download_forecast", forecast_table_data, "forecast_5yr.csv")
  
  #----------------------------------------------------------
  # Data Explorer
  #----------------------------------------------------------
  
  explorer_data <- reactive({
    switch(input$dataset_choice,
           "Country-level data (filtered)" = filtered_country(),
           "Country Rankings (filtered)" = country_rankings_data(),
           "Continent Summary (filtered)" = continent_summary_data(),
           "WHO Summary (year range)" = who_summary_data(),
           "Global Disease Summary (filtered)" = disease_summary_data())
  })
  
  output$data_explorer <- renderDT({
    datatable(explorer_data(), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  download_server(output, "download_data", explorer_data, "selected_dataset.csv")
}

shinyApp(ui, server)
