library(tidyverse)
library(tidytuesdayR)
library(countries)
library(ggtext)
library(showtext)
library(glue)
library(skimr)

# Loading the data from TT repo

tuesdata <- tt_load("2024-11-19")

episode_metrics <- tuesdata$episode_metrics

view(episode_metrics)
skim(episode_metrics) |> view()

# Plot aesthetics
bgcol <- 	"#F5F5DC"
textcol <- "#191970"
title <- "Sentiment Variance and Episode Length"
st <- "A Visualisation of sentiment variance and dialogue density with average length of the episodes
Graphic: Prasaath Sastha"

episode_metrics |> 
  ggplot(aes(sentiment_variance, avg_length))+ 
  geom_point(aes(colour = dialogue_density),size = 2, alpha = 0.5)+ 
  geom_smooth(aes(sentiment_variance), se = FALSE)+
  facet_wrap(~ season, labeller = label_both)+
  theme(
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.position = "inside",
    legend.position.inside = c(0.8,0.09),
    legend.background = element_rect(fill = bgcol),
    legend.key.size = unit(1.0, "cm"),
    plot.background = element_rect(fill = bgcol),
    panel.background = element_rect(fill = bgcol),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = bgcol),
    plot.title = element_text(
      size        = rel(2.3),
      family      = "title",
      face        = "bold",
      color       = textcol,
      margin      = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_text(
      size        = rel(1.05),
      family      = "subtitle",
      color       = textcol,
      lineheight  = 1.1,
      margin      = margin(t = 5, b = 15)
    ),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )+
  labs(
    color = "Dialogue Density",
    x = "Sentiment Variance",
    y = "Average Length",
    title = title,
    subtitle = st,
    caption = "Package: Bob's Burgers,
    #Tidy Tuesday"
  )
