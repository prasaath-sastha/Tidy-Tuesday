

library(tidyverse) # This is a collection of packages that is used for data exploration, wrangling and plotting.
library(tidytuesdayR) # This package is used to connect to the tidy Tuesday repo for the data
library(knitr)
# Library that plays a huge role in improving test plot aesthetics
library(patchwork)
library(ggrepel)
library(glue) 
library(ggtext) 
library(showtext)

# Libraries for Maps
library(tmap)
library(sf)
library(rnaturalearth)
library(camcorder)

tuesdata <- tt_load("2024-10-22")

cia_factbook <- tuesdata$cia_factbook

glimpse(cia_factbook)


# Creating some general variables for names and titles for axis and legenda

title_area <- "Area"
title_birth_rate <- "Birth Rate"
title_death_rate <- "Death Rate"
title_infantMR <- "Infant Mortality Rate"
title_maternalMR <- "Maternal Mortality Rate"
title_interetuser <- "Internet Users"
title_netmigration <- "Net Migration Rate"
title_population <- "Population"

t_population_growth_rate <- tibble(title = "Population Growth Rate")
t_area <- tibble(title = "Area", unit = "Sq Km")
t_birth_rate <- tibble(title = "Birth Rate", unit = "live births per 1,000 people")
t_death_rate <- tibble(title = "Death Rate", unit = "deaths per 1,000 people")
t_infantMR <- tibble(title = "Infant Mortality Rate", unit = "deaths of infants under one year old per 1,000 live births")
t_maternalMR <- tibble(title = "Maternal Mortality Rate", unit = "maternal deaths per 100,000 live births")
t_interetuser <- tibble(title = "Internet Users", unit = "n")
t_netmigration <- tibble(title = "Net Migration Rate", unit = "migrants per 1,000 people")
t_population <- tibble(title = "Population", unit = "n")

bg_col <- "white"

boxplot <- function(data, var, xlab = "X-Axis", ylab = "Y-Axis", main = "My box Plot", col = "blue") {
  # Create a dataframe from x and y
  
  # Generate the plot
  ggplot(data = data, aes(x = {{var}})) +
    geom_boxplot(color = col, outlier.colour = "red", na.rm = TRUE, orientation = "y")+
    scale_y_continuous(labels = NULL)+
    labs(x = xlab, y = ylab)
}

gsave <- function(number){
  ggsave(filename = paste0("image_", {{number}}, ".png"), device = "png", bg=bg_col)
}


gg_record(device = "png")

A <- boxplot(cia_factbook, area, xlab = paste(t_area$title, "(", t_area$unit, ")"), ylab = "")
B <- boxplot(cia_factbook, birth_rate , xlab = paste(t_birth_rate$title, "(", t_birth_rate$unit, ")"), ylab = "")
C <- boxplot(cia_factbook, death_rate, xlab = paste(t_death_rate$title, "(", t_death_rate$unit, ")"), ylab = "")
D <- boxplot(cia_factbook, infant_mortality_rate , xlab = paste(t_infantMR$title, "(", t_infantMR$unit, ")"), ylab = "")
E <- boxplot(cia_factbook, internet_users , xlab = paste(t_interetuser$title, "(", t_interetuser$unit, ")"), ylab = "")
F <- boxplot(cia_factbook, maternal_mortality_rate , xlab = paste(t_maternalMR$title, "(", t_maternalMR$unit, ")"), ylab = "")
G <- boxplot(cia_factbook, net_migration_rate , xlab = paste(t_netmigration$title, "(", t_netmigration$unit, ")"), ylab = "")
H <- boxplot(cia_factbook, population , xlab = paste(t_population$title, "(", t_population$unit, ")"), ylab = "")
I <- boxplot(cia_factbook, population_growth_rate , xlab = t_population_growth_rate$title, ylab = "")


A+B+C+D+E+F+G+H+I+ plot_layout(ncol = 2)


world <- ne_countries(scale = "medium", returnclass = "sf")

cia_world_data <- world |> 
  left_join(cia_factbook, by = c("name" = "country"))

map_cia <- function(data, var, title, value) {
  ggplot(data = data) +
    geom_sf(aes(fill = {{ var }}), color = "black", size = 0.1) + 
    scale_fill_continuous(type = "viridis", na.value = "grey90", name = value, limits = NULL) +
    labs(title = title, caption = "Countries with no data are shaded in grey") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.text.position = "bottom",
      legend.title.position = "top",
      legend.key.width = unit(2, "cm")
    )
}

map_cia(cia_world_data, internet_users, title= t_interetuser$title, value = t_interetuser$unit)
map_cia(cia_world_data, population, title = t_population$title, value = t_population$unit)
map_cia(cia_world_data, birth_rate, title = t_birth_rate$title, value = t_birth_rate$unit)
map_cia(cia_world_data, death_rate, title = t_death_rate$title, value = t_death_rate$unit)
map_cia(cia_world_data, infant_mortality_rate, title = t_infantMR$title, value = t_infantMR$unit)
map_cia(cia_world_data, maternal_mortality_rate, title = t_maternalMR$title, value = t_maternalMR$unit)
map_cia(cia_world_data, population_growth_rate, title = t_population_growth_rate$title, value = "")
map_cia(cia_world_data, net_migration_rate, title = t_netmigration$title, value = t_netmigration$unit)


top10 <- cia_factbook |> 
  mutate(
    internet_users_per_square_Km = internet_users/area,
    percent_internet_users = (internet_users/population)*100,
    .keep = "all"
  )

p1 <- top10 |> arrange(desc(internet_users_per_square_Km)) |> slice_head(n=10) |> ggplot(aes(x = reorder(country, internet_users_per_square_Km), y = internet_users_per_square_Km, fill = country))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  coord_flip()+
  labs(
    title = "Top 10 countries with most internet users per square Kilometer",
    y = "Number of internet users per square Kilometers",
    x = NULL
  )+
  geom_text(
    aes(label = round(internet_users_per_square_Km, 2)),
    position = position_dodge(width = 0.5)
  )+
  theme_minimal()

p2 <- top10 |> arrange(desc(percent_internet_users)) |> slice_head(n=10) |> ggplot(aes(x = reorder(country, percent_internet_users), y = percent_internet_users, fill = country))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  coord_flip()+
  labs(
    title = "Top 10 countries with most internet users",
    y = "Percentage of Internet Users",
    x = NULL
  )+
  geom_text(
    aes(label = round(percent_internet_users, 2)),
    position = position_dodge(width = 0.5)
  )+
  theme_minimal()

p1
p2

cia_internet_data <- world |> 
  left_join(top10, by = c("name" = "country"))

map_cia(cia_internet_data, internet_users_per_square_Km, title = "Internet users per square Kilometer", value = "Number of People") |> gsave(number = 11)
map_cia(cia_internet_data, percent_internet_users, title = "Percentage of Internet Users", value = "%") |> gsave(number = 12)

gg_stop_recording()
gg_playback(name = file.path(tempdir(), "recording", "22-10.24.gif"),
            first_image_duration = 4,
            last_image_duration = 12,
            frame_duration = .5,
            image_resize = 900,
            width = 800,
            height = 800)
