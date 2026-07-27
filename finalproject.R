# =====================================================================
# Title: COVID-19 India Analysis Shiny Dashboard
# Description: Server-based R Shiny application converting static 
#              visualizations, maps, and interactive plots into a dashboard.
# =====================================================================

# 1. Load Required Libraries ----
library(tidyverse)
library(plotly)
library(lubridate)
library(maps)
library(viridis)
library(reshape2)
library(readxl)
library(shiny)
library(leaflet)

# 2. Data Preparation & Preprocessing ----
# Load dataset
covid <- read_excel("~/Downloads/COVID19_India_Capstone_Dataset.xlsx")

# Calculate Recovery Rate
covid <- covid |>
  mutate(RecoveryRate = (Recovered / Confirmed_Cases) * 100)

# Define Geographic Coordinates for Mapping
state_map <- data.frame(
  State = c("Andhra Pradesh", "Telangana", "Karnataka", "Tamil Nadu",
            "Kerala", "Maharashtra", "Gujarat", "Delhi",
            "West Bengal", "Uttar Pradesh"),
  lat = c(15.9129, 17.3850, 15.3173, 11.1271,
          10.8505, 19.7515, 22.2587, 28.7041,
          22.9868, 26.8467),
  lng = c(79.7400, 78.4867, 75.7139, 78.6569,
          76.2711, 75.7139, 71.1924, 77.1025,
          87.8550, 80.9462)
)

# Aggregate map data
map_data <- covid %>%
  group_by(State) %>%
  summarise(TotalDeaths = sum(Deaths)) %>%
  left_join(state_map, by = "State")

# 3. User Interface (UI) ----
ui <- fluidPage(
  titlePanel("COVID-19 India Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Dashboard Controls"),
      helpText("This dashboard displays comprehensive visualizations of COVID-19 trends across Indian states, including case distributions, recovery rates, correlation matrices, and geographical maps.")
    ),
    
    mainPanel(
      width = 9,
      fluidRow(
        column(6, plotOutput("stateCasesPlot", height = "400px")),
        column(6, plotOutput("stateDeathsPlot", height = "400px"))
      ),
      hr(),
      fluidRow(
        column(12, plotOutput("newCasesPlot", height = "350px"))
      ),
      hr(),
      fluidRow(
        column(6, plotOutput("recoveryRatePlot", height = "400px")),
        column(6, plotOutput("activeHistPlot", height = "400px"))
      ),
      hr(),
      fluidRow(
        column(6, plotOutput("activeBoxPlot", height = "400px")),
        column(6, plotOutput("positivityDensityPlot", height = "400px"))
      ),
      hr(),
      fluidRow(
        column(6, plotOutput("testsVsCasesPlot", height = "400px")),
        column(6, plotOutput("corrHeatmapPlot", height = "400px"))
      ),
      hr(),
      fluidRow(
        column(12, plotlyOutput("scatterPlotly", height = "500px"))
      ),
      hr(),
      fluidRow(
        column(6, leafletOutput("leafletMap", height = "500px")),
        column(6, plotlyOutput("geoPlotly", height = "500px"))
      )
    )
  )
)

# 4. Server Logic ----
server <- function(input, output, session) {
  
  # Total Cases by State Bar Plot
  output$stateCasesPlot <- renderPlot({
    covid |>
      group_by(State) |>
      summarise(TotalCases = sum(Confirmed_Cases)) |>
      ggplot(aes(reorder(State, TotalCases), TotalCases, fill = State)) +
      geom_col() +
      coord_flip() +
      labs(title = "Total COVID Cases by State", x = "State", y = "Total Cases") +
      theme_minimal()
  })
  
  # Total Deaths by State Bar Plot
  output$stateDeathsPlot <- renderPlot({
    covid |>
      group_by(State) |>
      summarise(TotalDeaths = sum(Deaths)) |>
      ggplot(aes(reorder(State, TotalDeaths), TotalDeaths, fill = State)) +
      geom_col() +
      coord_flip() +
      labs(title = "Total COVID Deaths by State", x = "State", y = "Total Deaths") +
      theme_minimal()
  })
  
  # New Cases Over Time Line Plot
  output$newCasesPlot <- renderPlot({
    covid |>
      group_by(Date) |>
      summarise(NewCases = sum(New_Cases)) |>
      ggplot(aes(Date, NewCases)) +
      geom_line(color = "red", linewidth = 1) +
      geom_point(color = "darkred", size = 1.5) +
      labs(title = "New COVID Cases Over Time", x = "Date", y = "New Cases") +
      theme_minimal()
  })
  
  # Recovery Rate by State Bar Plot
  output$recoveryRatePlot <- renderPlot({
    covid |>
      group_by(State) |>
      summarise(Recovery = mean(RecoveryRate)) |>
      ggplot(aes(reorder(State, Recovery), Recovery, fill = State)) +
      geom_col() +
      coord_flip() +
      labs(title = "Average Recovery Rate by State", x = "State", y = "Recovery Rate (%)") +
      theme_minimal()
  })
  
  # Active Cases Histogram
  output$activeHistPlot <- renderPlot({
    ggplot(covid, aes(Active_Cases)) +
      geom_histogram(binwidth = 10000, fill = "steelblue", color = "white") +
      labs(title = "Distribution of Active Cases", x = "Active Cases", y = "Frequency") +
      theme_minimal()
  })
  
  # Active Cases Boxplot by State
  output$activeBoxPlot <- renderPlot({
    ggplot(covid, aes(State, Active_Cases, fill = State)) +
      geom_boxplot() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title = "Active Cases Distribution by State", x = "State", y = "Active Cases")
  })
  
  # Positivity Rate Density Plot
  output$positivityDensityPlot <- renderPlot({
    ggplot(covid, aes(Positivity_Rate)) +
      geom_density(fill = "orange", alpha = 0.6) +
      labs(title = "Positivity Rate Density", x = "Positivity Rate", y = "Density") +
      theme_minimal()
  })
  
  # Tests Conducted vs New Cases Scatter Plot
  output$testsVsCasesPlot <- renderPlot({
    ggplot(covid, aes(Tests_Conducted, New_Cases, color = State)) +
      geom_point(alpha = 0.7, size = 2) +
      labs(title = "Tests Conducted vs New Cases", x = "Tests Conducted", y = "New Cases") +
      theme_minimal()
  })
  
  # Correlation Heatmap
  output$corrHeatmapPlot <- renderPlot({
    corr <- covid |>
      select(Confirmed_Cases, Recovered, Deaths, Vaccinated, Tests_Conducted, Positivity_Rate)
    
    corr_matrix <- cor(corr, use = "complete.obs")
    corr_long <- melt(corr_matrix)
    
    ggplot(corr_long, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Feature Correlation Heatmap", x = "", y = "", fill = "Pearson Cor")
  })
  
  # Interactive Plotly Scatter Plot (Cases vs Deaths)
  output$scatterPlotly <- renderPlotly({
    p <- ggplot(covid, aes(Confirmed_Cases, Deaths, color = State,
                           text = paste("State:", State,
                                        "<br>Cases:", Confirmed_Cases,
                                        "<br>Deaths:", Deaths))) +
      geom_point(size = 2, alpha = 0.8) +
      theme_minimal() +
      labs(title = "Confirmed Cases vs Deaths", x = "Confirmed Cases", y = "Deaths")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Leaflet Map Implementation
  output$leafletMap <- renderLeaflet({
    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = ~TotalDeaths / 50000,
        color = "red",
        fillColor = "darkred",
        fillOpacity = 0.7,
        stroke = TRUE,
        popup = ~paste(
          "<b>State:</b>", State,
          "<br><b>Total Deaths:</b>", TotalDeaths
        )
      )
  })
  
  # Plotly Geo Map Implementation
  output$geoPlotly <- renderPlotly({
    plot_ly(
      data = map_data,
      type = "scattergeo",
      mode = "markers",
      lat = ~lat,
      lon = ~lng,
      text = ~paste("State:", State, "<br>Deaths:", TotalDeaths),
      hoverinfo = "text",
      marker = list(
        size = 15,
        color = ~TotalDeaths,
        colorscale = "Reds",
        showscale = TRUE
      )
    ) %>%
      layout(
        title = "COVID-19 India State Deaths Map",
        geo = list(
          scope = "asia",
          showland = TRUE,
          landcolor = "rgb(240,240,240)",
          showcountries = TRUE,
          countrycolor = "black",
          center = list(lat = 22, lon = 80),
          projection = list(type = "mercator"),
          lataxis = list(range = c(6, 38)),
          lonaxis = list(range = c(68, 98))
        )
      )
  })
}

# 5. Run the Application ----
shinyApp(ui = ui, server = server)


getwd()