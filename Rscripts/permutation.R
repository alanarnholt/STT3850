worms <- c(17, 16, 10, 10, 7, 10, 7, 2, 2, 1)
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
mean(pd$stat >= 7.6)


set.seed(321)
ic |> 
  specify(formula = worms ~ treat) |> 
  # specify(response = worms, explanatory = treat) |> 
  hypothesize(null = "independence") |> 
  generate(reps = 10^4, type = "permute") |> 
  calculate(stat = "t", order = c("no", "yes")) -> pd2
pd2
visualize(pd2)
obs_t <- t.test(worms~treat, data = ic, var.equal = TRUE)$stat
get_p_value(pd2, obs_stat = obs_t, direction = "greater")

## Note t = {(xbar1 - xbar2) - (mu1 - mu2)}/{sp*sqrt(1/n1 + 1/n2)}
## where sp2 = {(n1 - 1)s1^2 + (n2 - 1)S2^2}/(n1 + n2 -2)
ic |> 
  group_by(treat) |> 
  summarize(n = n(), xbar = mean(worms), SD = sd(worms)) -> SUM
SUM
(sp2 <- ( (5 - 1)*4.30^2 + (5 - 1)*3.91^2 )/(5 + 5 -2))
(sp <- sqrt(sp2))
(tstar <- (12 - 4.4)/(sp*sqrt(1/5 + 1/5)) )
# Note that the value is not quite the same due to our rounding....
(s1 <- SUM$SD[1])
(s2 <- SUM$SD[2])
(sp2 <- ((5 - 1)*s1^2 + (5 - 1)*s2^2)/(5 + 5 -2))
(sp <- sqrt(sp2))
(tstar2 <- (12 - 4.4)/(sp*sqrt(1/5 + 1/5)) ) 
  
  
  
  
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
