library(tidyverse) # This is a collection of packages that is used for data exploration, wrangling and plotting.
library(tidytuesdayR) # This package is used to connect to the tidy Tuesday repo for the data

# Library that plays a huge role in improving test plot aesthetics

library(ggrepel)
library(glue) 
library(ggtext) 
library(showtext)
library(ggridges)


tuesdata <- tt_load("2024-11-05")
democract_raw <- tuesdata$democracy_data
view(democract_raw)

# Define the questions or the visualization plan for this tidy Tuesday.
# To count the years each dictator served a country in each continent. 
# Steps: First group the data based on the different dictators of each country, and summarize the number of years they were
# dictators and the group the list of dictators based on the continent. The plot can be a circle and the years can be
# used for the size attribute, Label the dictators (atleast the top five) in each continent.

#

female_pre <- democract_raw |> group_by(country_name, is_female_president) |> 
                            summarise(count = n()) |> 
                            filter(is_female_president == "TRUE") |> 
                            select(country_name, count)
female_mon <- democract_raw |> group_by(country_name, is_female_monarch) |> 
  summarise(count_m = n()) |> 
  filter(is_female_monarch == "TRUE") |> 
  select(country_name, count_m)

world <- ne_countries(scale = "medium", returnclass = "sf")
ang <- ne_countries(country = 'Anguilla', type = 'countries', returnclass = "sf")


map_data_1 <- world |> left_join(female_pre, by = c("name" = "country_name"))
map_data_2 <- map_data_1 |> left_join(female_mon, by = c("name" = "country_name"))


ggplot(data = map_data_2)+
  geom_sf(aes(fill = count))

democract_raw |> group_by(country_name) |> filter(is_monarchy == "TRUE")

president_gender_1 <-  democract_raw |> mutate(
  president_gender = case_when(
    is_female_president == "TRUE" ~ "Female",
    is.na(is_female_president) ~ "NA",
    .default = "Male"
  )
)

president_gender_1 |> group_by(president_gender) |> summarise(count = n())

A <- "red"
B <- "green"
cl <- "grey"

plot_color <- c("red", "green", "grey")

title <- "Presidential governance density over the years"

st <- glue("Density plot showing distribution of presidential genders: 
            <span style='color:{A}'>**Female**</span>, <span style='color:{B}'>**Male**</span>, and <span style='color:{cl}'>**Unavailable Data**</span> across 
           years from 1950 to 2020<br><br> Graphic: Prasaath Sastha")

ggplot(president_gender_1, aes(x = year))+
  geom_density(aes(fill = president_gender, alpha = 0.2), show.legend = FALSE)+
  theme_minimal()+
  scale_fill_manual(values = plot_color)+
  labs(
    x = NULL,
    y = NULL,
    title = title,
    subtitle = st
  )+
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.spacing = unit(.4, "lines"),
    plot.title = element_textbox_simple(
      colour = "black",
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      lineheight = 0.5,
      maxwidth = 0.9,
      size = rel(2)
    ),
    plot.subtitle = element_textbox_simple(
      colour = "grey22",
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.45,
      maxwidth = 1,
      size = rel(1)
    ),
    axis.text.y = element_blank()
  )

ggsave("05NOV2024.png")
