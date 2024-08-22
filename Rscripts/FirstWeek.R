# First Week stuff
library(tidyverse)
library(palmerpenguins)
penguins
ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) + 
  geom_point() + 
  geom_smooth(method = "lm") 

## Suppose we want to work with only Chinstrap

penChin <- penguins |> 
  filter(species == "Chinstrap")
penChin

ggplot(data = penChin, aes(x = bill_length_mm, y = bill_depth_mm)) + 
  geom_point(color = "red") + 
  geom_smooth(method = "lm", color = "purple") +
  theme_classic() + 
  labs(x = "Bill length in mm", y = "Bill depth in mm",
       title = "You put something here",
       subtitle = "Where did the data come from?",
       caption = "Data: penguins")
