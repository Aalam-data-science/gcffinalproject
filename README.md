# COVID-19 India Analysis Shiny Dashboard 🦠🇮🇳

An interactive, server-based R Shiny application designed to track, analyze, and visualize COVID-19 data across various states in India. This dashboard transforms raw dataset metrics into comprehensive static visualizations, dynamic charts, and geospatial maps.

## 📊 Features

- **Geospatial Mapping:** Interactive maps using `Leaflet` and `Plotly` to visualize total deaths by state.
- **Trend Analysis:** Time-series line plots tracking new cases over time.
- **State-wise Comparisons:** Bar charts and boxplots comparing total cases, deaths, recovery rates, and active cases across states.
- **Statistical Insights:** 
  - Positivity rate density distributions.
  - Scatter plots analyzing the relationship between tests conducted and new cases.
  - Pearson correlation heatmap for features like confirmed cases, recoveries, deaths, vaccinations, and tests.
- **Interactive Visualizations:** Hover-enabled tooltips on scatter plots and maps for detailed data exploration.

## 🛠️ Tech Stack & Libraries

**Language:** R
**Framework:** Shiny
**Key Libraries:**
- `shiny`, `rsconnect` (App framework and deployment)
- `tidyverse`, `reshape2`, `lubridate`, `readxl` (Data manipulation & ingestion)
- `ggplot2`, `plotly`, `viridis` (Data visualization)
- `leaflet`, `maps` (Geospatial mapping)

## 🚀 How to Run Locally

1. **Clone the repository:**
   ```bash
   git clone [https://github.com/Aalam-data-science/gcffinalproject.git]
