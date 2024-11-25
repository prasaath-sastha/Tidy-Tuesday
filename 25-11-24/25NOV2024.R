# Load Libraries
library(tidyverse)
library(glue)
library(ggtext)
library(tidytuesdayR)
library(skimr)
library(magick)
library(grid)

tues_data <- tt_load(2024, week = 48)

cbp_resp <- tues_data$cbp_resp
cbp_state <- tues_data$cbp_state

skim(cbp_resp) |> view()
skim(cbp_state) |> view()
glimpse(cbp_state)


#Plot aesthetics
bg_col <- "#ffe3dd"
bg_col_1 <- "#f4f1f1"
text_col_1 <- "#0c7b24"



title <- "U.S. Customs and Border Protection (CBP) encounter"
st <- glue(" A breakdown of encounter counts by group: <span style='color:{A}'>**Accompanied Minors**</span>, <span style='color:{B}'>**Individual in a Family Unit (FMUA)**</span>, <span style='color:{C}'>**Single Adults**</span>, and <span style='color:{D}'>**Unaccompanied  Children / Single Minors**</span> from 2020 to 2024
           \n\n <span style='color:{E}'>**Graphic: Prasaath Sastha**</span>")

A <- "#ff751a"
B<- "#00cb79" 
C<- "#5075f3"
D<- "#9320b6"
E <- "#e81671"

plot_col <- c(
"Accompanied Minors" <- "#ff751a",
"FMUA" <- "#00cb79",
"Single Adults" <- "#5075f3",
"UC / Single Minors" <- "#9320b6")

cbp_state |> ggplot(aes(fiscal_year, encounter_count, colour = demographic))+
  geom_point(aes(alpha = 0.5), show.legend = FALSE)+
  geom_jitter()+
  scale_color_manual(values = plot_col)+
  facet_wrap(~ demographic)+
  guides(color = "none")+
  labs(
    title = title,
    subtitle = st,
    caption = "Week = 48,
    Tidy Tuesday"
  )+
  theme(
    plot.background = element_rect(fill = bg_col),
    panel.background = element_rect(fill = bg_col),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.title = element_text(
      size        = rel(2),
      face        = "bold",
      color       = text_col_1,
      margin      = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_textbox_simple(
      size        = rel(1.2),
      color       = text_col_1,
      lineheight  = 1.1,
      margin      = margin(t = 5, b = 15)
    ),
    axis.title = element_blank()
  )
