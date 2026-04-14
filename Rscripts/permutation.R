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



set.seed(321)
ic |> 
  specify(formula = worms ~ treat) |> 
  # specify(response = worms, explanatory = treat) |> 
  hypothesize(null = "independence") |> 
  generate(reps = 10^4, type = "permute") |> 
  calculate(stat = "t", order = c("no", "yes")) -> pd2
pd2
obs_t <- t.test(worms~treat, data = ic, var.equal = TRUE)$stat
get_p_value(pd2, obs_stat = obs_t, direction = "greater")

##### With a for loop
P <- 10^4
tstat <- numeric(P)
for(i in 1:P){
  tstat[i] <- t.test(worms~sample(treat), data = ic, var.equal = TRUE)$stat
}
p_val <- mean(tstat >= obs_t)
p_val






### Test mu_male_hours - mu_female_hours = 0 vs >

obs_diff <- -diff(tapply(gss$hours, gss$sex, mean))
obs_diff
gss |> 
  specify(hours ~ sex) |> 
  hypothesize(null = "independence") |> 
  generate(reps = 10^4, type = "permute") |> 
  calculate(stat = "diff in means", order = c("male", "female")) -> pd
head(pd)
visualize(pd)
get_p_value(pd, obs_stat = obs_diff, direction = "greater")
