library(moderndive)
library(tidyverse)
glimpse(evals)

ggplot(data = evals, aes(x = age, y = score, color = gender)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()


ggplot(data = evals, aes(x = age, y = score, color = gender)) + 
  geom_point() + 
  geom_parallel_slopes(se = FALSE) + 
  theme_bw()
