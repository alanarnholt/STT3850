# Chapter 7 MD

library(moderndive)
library(tidyverse)
set.seed(123)
bowl
virtual_sample <- bowl |> 
  rep_sample_n(size = 49)
virtual_sample
# Proportion Red
virtual_sample |> 
  mutate(is_red = (color =="red")) |> 
  summarize(num_red = sum(is_red)) |> 
  mutate(prop_red = num_red / 49)

# Use the virtual shovel 1000 times
set.seed(34)
virtual_samples <- bowl |> 
  rep_sample_n(size = 49, reps = 1000)
virtual_samples

virtual_prop_red <- virtual_samples |> 
  group_by(replicate) |> 
  summarize(prop_red = mean(color == "red"))
virtual_prop_red

ggplot(data = virtual_prop_red, aes(x = prop_red)) + 
  geom_histogram()
#
ggplot(data = virtual_prop_red, aes(x = prop_red)) + 
  geom_histogram(binwidth = 0.02, color = "black", fill = "purple") + 
  theme_bw()

# Take Note:
virtual_prop_red |> 
  summarize(M_p = mean(prop_red), S_p = sd(prop_red))
# Note how this compares to
bowl |> 
  summarize(mean(color == "red"), sd(color == "red")/sqrt(49))
