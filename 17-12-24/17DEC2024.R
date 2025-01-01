library(tidyverse)
library(tidytuesdayR)

library(packcircles)

tues_data <- tt_load(2024, week = 51)

spells_data <- tues_data$spells

view(spells)


# Select relevant columns for classes and count spells for each class
class_columns <- c("bard", "cleric", "druid", "paladin", "ranger", "sorcerer", "warlock", "wizard")
class_counts <- spells_data |> 
  select(class_columns) |> 
  summarise(across(everything(), sum)) |> 
  pivot_longer(cols = everything(), names_to = "Class", values_to = "SpellCount")

# Add spell school for visualization (optional grouping)
class_counts <- class_counts |> 
  mutate(School = "All Schools") # Add school grouping if desired

# Generate circle packing layout
packing <- circleProgressiveLayout(class_counts$SpellCount, sizetype = "area")
class_counts <- cbind(class_counts, packing)
circle_data <- circleLayoutVertices(packing, npoints = 100)

# Create circular packing visualization
ggplot() +
  geom_polygon(data = circle_data, aes(x, y, group = id, fill = as.factor(id)), color = "black", alpha = 0.7) +
  geom_text(data = class_counts, aes(x, y, label = Class), size = 4) +
  scale_fill_viridis_d(option = "C") + # Use viridis color palette
  coord_equal() +
  labs(
    title = "Spell Distribution by Class",
    subtitle = "Each circle represents the count of spells available to a class",
    caption = "#TidyTuesday, Week 51",
    x = NULL,
    y = NULL,
    fill = "Class"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "none"
  )


# Plot 2 ------------------------------------------------------------------

library(circlize)



# Prepare data for the chord diagram
# Summarize the count of spells by level and school
level_school_matrix <- spells_data |> 
  group_by(level, school) |> 
  summarise(SpellCount = n()) |> 
  ungroup() |> 
  pivot_wider(names_from = school, values_from = SpellCount, values_fill = 0) |> 
  column_to_rownames("level") |> 
  as.matrix()

# Create the chord diagram
chordDiagram(
  level_school_matrix,
  transparency = 0.5,
  annotationTrack = "grid",
  preAllocateTracks = 1
)

# Add annotations
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

# Add title
title("Spell Levels and Schools")
