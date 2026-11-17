library(tidyverse)
library(infer)
library(moderndive)
movies_sample
tapply(movies_sample$rating, movies_sample$genre, mean)
movies_sample |> 
  group_by(genre) |> 
  summarize(M = mean(rating), n = n())
movies_sample |> 
  specify(rating ~ genre) |> 
  calculate(stat = "diff in means", order = c("Romance", "Action"))
# Permutation Null Distribution
movies_sample |> 
  specify(rating ~ genre) |> 
  hypothesize(null = "independence") |> 
  generate(reps = 10^4, type = "permute") |> 
  calculate(stat = "diff in means", order = c("Romance", "Action")) -> PD
visualize(PD) + 
  shade_p_value(obs_stat = 1.05, direction = "two_sided")
get_p_value(PD, obs_stat = 1.05, direction = "two_sided")
