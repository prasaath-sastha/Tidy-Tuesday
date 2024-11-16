library(tidyverse)
library(tidytuesdayR)
library(countries)
library(ggtext)
library(showtext)
library(glue)
library(plotly)

# Loading the data from TT repo

tuesdata <- tt_load("2024-11-12")

countries <- tuesdata$countries
country_subdiv <- tuesdata$country_subdivisions
former_countries <- tuesdata$former_countries


title <- "Global Interactive Visualization of Countries Using ISO Alpha-3 Codes and Numeric Codes"


quick_map(data = countries, plot_col = "numeric")

map <- plot_ly(
  type = 'choropleth',
  locations = countries$alpha_3,  
  z = countries$numeric,          
  text = countries$name,        
  colorscale = '', 
  showscale = FALSE,
  marker = list(line = list(color = 'white', width = 0.5))
)

# Layout adjustments
map <- map %>%
  layout(
    title = "Global Interactive Visualization of Countries Using 
    ISO Alpha-3 Codes and Numeric Codes",
    geo = list(
      projection = list(type = 'equirectangular'),
      showcoastlines = TRUE,
      coastlinecolor = "black",
      showframe = FALSE
    )
  )


map