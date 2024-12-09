# Load Libraries
library(tidyverse)
library(tidytuesdayR)

tues_data <- tt_load(2024, week = 49)

trafic_data <- tues_data$A64_traffic |> janitor::clean_names()

view(trafic_data)
skim(trafic_data) |> view()

trafic_data |> group_by(date = as.Date(report_date)) |> 
  summarise(avg_speed = mean(avg_mph, na.rm = TRUE))

hourly_data <- trafic_data |>
  group_by(report_date, hour = lubridate::hour(time_period_ending)) |>
  summarise(
    hourly_volume = sum(total_volume, na.rm = TRUE),
    avg_speed = mean(avg_mph, na.rm = TRUE),
    .groups = 'drop'
  )


ggplot(hourly_data, aes(x = hour, y = hourly_volume))+
  geom_hex(show.legend = FALSE)+
  labs(
    x = "Time(Hour)",
    y = "Volume",
    title = "A64 Traffic Volume",
    subtitle = "A simple overview of hourly flow of traffic on A64 over May 2021"
  )+
  theme(
    plot.background = element_rect(colour = "grey21"),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    title = element_text(face = "bold", size = 14)
  )
