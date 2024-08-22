# First Week stuff
library(tidyverse)
library(palmerpenguins)
penguins
str(penguins) 

mean(penguins$bill_length_mm, na.rm = TRUE)

penguins |> 
  filter(species == "Adelie") -> penAD
penAD

ggplot(data = penguins, aes(x = bill_length_mm, 
                            y = bill_depth_mm,
                            color = species)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

