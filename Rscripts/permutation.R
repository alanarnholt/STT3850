worms <-c(17, 16, 10, 10, 7, 10, 7, 2, 2, 1)
treat <- c(rep("no", 5), rep("yes", 5))
ic <- tibble(worms, treat)

ic
library(tidyverse)
ic |> 
  group_by(treat) |> 
  summarize(AW = mean(worms)) -> tab
tab
obs_diff <- tab$AW[1] - tab$AW[2]
obs_diff

set.seed(321)
P <- 10^4
md <- numeric(P)
for(i in 1:P){
  md[i] <- -diff(tapply(worms, sample(treat), mean))
}
hist(md)
(pvalue <- mean(md >= obs_diff))

# another approach
set.seed(321)
P <- 10^4
md2 <- numeric(P)
for(i in 1:P){
  junk <- sample(10, 5)
  md2[i] <- mean(worms[junk]) - mean(worms[-junk])
}
hist(md2)
(pv2 <- mean(md2 >= obs_diff))

#####
## using infer now
library(infer)
set.seed(321)
ic |> 
  specify(formula = worms ~ treat) |> 
  # specify(response = worms, explanatory = treat) |> 
  hypothesize(null = "independence") |> 
  generate(reps = 10^4, type = "permute") |> 
  calculate(stat = "diff in means", order = c("no", "yes")) -> pd
pd
get_p_value(pd, obs_stat = obs_diff, direction = "greater")


### Test mu_male_hours - mu_female_hours = 0 vs >
gss |> 
  specify(hours ~ sex) |> 
  hypothesize(null = "independence") |> 
  generate(reps = 10^4, type = "permute") |> 
  calculate(stat = "diff in means", order = c("male", "female")) -> pd
head(pd)
visualize(pd)
