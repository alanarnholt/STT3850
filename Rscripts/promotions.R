# Two proportions
# H_0: p_m - p_f = 0
# H_A: p_m - p_f > 0

library(infer)
library(moderndive)
library(tidyverse)
head(promotions)

# Compute the observed diff in proportions
promotions |> 
  specify(decision ~ gender, success = "promoted") |> 
  calculate(stat = "diff in props", order = c("male", "female")) -> obs
obs
obs$stat

# Generate null distribution
promotions |> 
  specify(decision ~ gender, success = "promoted") |> 
  hypothesize(null = "independence") |> 
  generate(reps = 10^4, type = "permute") |> 
  calculate(stat = "diff in props", order = c("male", "female")) -> PD
PD  
## Visualize distribution
visualize(PD) +
  shade_p_value(obs_stat = obs$stat, direction = "greater")

# Get p-value
get_p_value(PD, obs_stat = obs$stat, direction = "greater")

# English Conclusion, type of error.

# CI
promotions |> 
  specify(decision ~ gender, success = "promoted") |> 
  generate(reps = 10^4, type = "bootstrap") |> 
  calculate(stat = "diff in props", order = c("male", "female")) -> BD
visualize(BD)
get_confidence_interval(BD, level = 0.95) -> CIEP
CIEP
visualize(BD) +
  shade_confidence_interval(endpoints = CIEP, fill = "purple", color = "black") + 
  theme_bw()

# Is the 95% CI consistent with your test conclusion at an alpha level of 0.05?

xtabs(~decision + gender, data = promotions) -> T1
T1
prop.table(T1, 2)
(obs_p_diff <- -diff(prop.table(T1, 2)[2,]))

prop.test(x = c(21, 14), n = c(24, 24), alternative = "greater")
