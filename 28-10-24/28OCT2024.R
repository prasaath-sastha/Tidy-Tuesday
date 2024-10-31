

library(tidyverse) # This is a collection of packages that is used for data exploration, wrangling and plotting.
library(tidytuesdayR) # This package is used to connect to the tidy Tuesday repo for the data
library(knitr)
# Library that plays a huge role in improving test plot aesthetics
library(patchwork)
library(ggrepel)
library(glue) 
library(ggtext) 
library(showtext)

# Trying out new package - camcorder
library(camcorder)
gg_stop_recording()

gg_record(dir = tempdir(),
          device = "png",
          dpi = 300)

tuesdata <- tt_load("2024-10-29")

monster_movies <- tuesdata$monster_movies
monster_movie_genres <- tuesdata$monster_movie_genres



hor_mov <-monster_movies |> filter(str_detect(genres, "Horror"))

plot_color <-  c('Mix' = "violet",
                 'Non Horror' = "orange",
                 'Strictly Horror' = "red")

#Label color
Mix <-  "violet"
NonHorror <-  "orange"
StrictlyHorror <-  "red"

mov_class <- monster_movies |> mutate(
  mov_typ = case_when(
    str_detect(genres, paste0("\\b", "Horror,", "\\b")) ~ "Mix",
    str_detect(genres, paste0("\\b", ",Horror", "\\b")) ~ "Mix",
    str_detect(genres, paste0("\\b", "Horror", "\\b")) ~ "Strictly Horror",
    .default = "Non Horror"
  ),
  runtime_cat = case_when(runtime_minutes<=30 ~ "Very Short",
                          runtime_minutes>120 ~ "Very Long",
                          is.na(runtime_minutes) ~ "Missing",
                          .default = "Regular Movie")
) 

title <- "Movie Horror - Happy Halloween 🎃"
st <- glue("This plot examines the relationship between movie runtime, 
           genre, and viewer ratings in the Tidy Tuesday film dataset. 
           Movies are categorized by runtime into three groups: 
           Regular Movies, Very Long, and Very Short, and by 
           horror classification: <span style='color:{Mix}'>**Mix**</span> 
           (movies include a horror element), <span style='color:{NonHorror}'>**Non-Horror**</span> (no horror elements), 
           and <span style='color:{StrictlyHorror}'>**🎃Strictly Horror🎃**</span> . 
           ")
st <- paste0(
  st, "<br><br>**Data**:  Internet Movie Database <br>**Graphic**: ", "Prasaath-Sastha"
)

mov_class |> 
  filter(runtime_cat == c("Very Short", "Regular Movie", "Very Long")) |>   
  ggplot(aes(average_rating, runtime_minutes, size = 3, color = mov_typ))+
  geom_point(show.legend = FALSE, aes(size = num_votes))+
  facet_grid(mov_typ~runtime_cat)+
  scale_color_manual(values = plot_color)+
  coord_cartesian(ylim = c(0, 200))+
  labs(
    title = title,
    subtitle = st,
    x = "Average Rating",
    y = "Runtime (mins)"
  )+
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.spacing = unit(.4, "lines"),
    plot.title = element_textbox_simple(
      colour = "orange",
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
    )
  )

ggsave("tt_28-10-24.png", plot = last_plot(), device = "png", width = 5, height = 6)

gg_stop_recording()

gg_playback()