library(tidyverse)
library(resampledata)
library(infer)
library(moderndive)
chlorine <- Bangladesh |>
  select(Chlorine) |>
  pull()
chlorine
chlorine <- chlorine[!is.na(chlorine)]
summary(chlorine)
set.seed(123)
t.test(chlorine)$conf

xbar <- mean(chlorine)
S <- sd(chlorine)
n <- length(chlorine)
###
B <- 10^5
tstar <- numeric(B)
cen <- numeric(B)
for(i in 1:B){
  bss <- sample(chlorine, size = n, replace = TRUE)
  cen[i] <- mean(bss)
  tstar[i] <- (mean(bss) - xbar)/(sd(bss)/sqrt(n))
}
Q <- quantile(tstar, probs = c(0.025, 0.975))
Q

CIpercentile <- quantile(cen, probs = c(0.025, 0.975))
CIpercentile
CItstar <- c(xbar - Q[2]*S/sqrt(n), xbar - Q[1]*S/sqrt(n))
CItstar

t.test(chlorine)$conf

Worms <- c(17, 16, 10, 10, 7, 10, 7, 2, 2, 1)

index <- sample(10, 5, replace = FALSE)
(sim_diff <- mean(Worms[index]) - mean(Worms[-index]))



library(tidyverse) 
library(infer) 
library(moderndive) 
library(nycflights13) 
library(ggplot2movies)

promotions |> 
  group_by(gender) |> 
  summarize(p = mean(decision == "promoted")) -> ans1
ans1
(pd <- -diff(ans1$p))





promotions
tapply(promotions$decision == "promoted", promotions$gender, mean)
(obs_diff <- -diff(tapply(promotions$decision == "promoted", promotions$gender, mean)))

B <- 10^5-1
pd <- numeric(B)
for(i in 1:B){
  pd[i] <- -diff(tapply(promotions$decision == "promoted", sample(promotions$gender), mean))
}
hist(pd)
(pv <- (sum(pd >= obs_diff) + 1)/(B + 1))

#####
promotions |> 
  specify(decision ~ gender, success = "promoted") |> 
  hypothesize(null = "independence") |> 
  generate(reps = 10^5 - 1, type = "permute") |> 
  calculate(stat = "diff in props", order = c("male", "female")) -> pd
pd
get_pvalue(pd, obs_stat = obs_diff, direction = "right")
((sum(pd$stat >= obs_diff) + 1)/(B + 1) -> pv2)

####### class stuff from 11/19/2024
#### Bangladesh

# Want to test H_0: mu = 0.45 vs H_A: mu > 0.45
library(resampledata)
Bangladesh |> 
  summarize(MC = mean(Cobalt, na.rm = TRUE))
# t.test(Bangladesh$Cobalt, alte = "greater", mu = 0.45)
Bangladesh$Cobalt[!is.na(Bangladesh$Cobalt)] -> cobalt
(xbar <- mean(cobalt))
(delta <- 0.45 - xbar)
B <- 10^5 - 1
xb <- numeric(B)
for(i in 1:B){
  bss <- sample(cobalt, size = length(cobalt), replace = TRUE) + delta
  xb[i] <- mean(bss)
}
hist(xb)
(pvalue <- (sum(xb >= xbar) + 1)/(B + 1))

### Same thing with infer
Bangladesh |> 
  specify(response = Cobalt) |> 
  hypothesize(null = "point", mu = 0.45) |> 
  generate(reps = 10^5) |> 
  calculate(stat = "mean") -> pd
pd |> 
get_p_value(obs_stat = xbar, direction = "right")


#######
## Lets Test H_0: p_male = 0.7 vs H_A: p_male > 0.7
library(moderndive)
promotions |> 
  filter(gender == "male") -> pm
(mean(pm$decision=="promoted") -> phat)

pm |>
  specify(response = decision, success = "promoted") |>
  hypothesize(null = "point", p = 0.7) |>
  generate(reps = 10^4, type = "draw") |>
  calculate(stat = "prop") -> pd
visualize(pd)
get_pvalue(pd, obs_stat = phat, direction = "right")


(mean(pm$decision=="promoted") -> phat)


B <- 10^4 - 1
ps <- numeric(B)
for(i in 1:B){
  bss <- sample(c(0,1), size = 24, replace = TRUE, prob = c(0.30, 0.70))
  ps[i] <- mean(bss)
}
hist(ps, breaks = 20)
summary(ps)
(pvalue <- (sum(ps >= phat) + 1)/(B + 1))
mean(ps)

## Stuff from 11/21/2024
## Test: H_0: mu_ale(alcohol) = mu_lager(alcohol) vs not equal
Alelager |> 
  ggplot(aes(Alcohol)) + 
  geom_density() + 
  facet_grid(rows = vars(Type))

Alelager |> 
  ggplot(aes(sample = Alcohol)) + 
  geom_qq_line() + 
  geom_qq() +
  facet_grid(rows = vars(Type))

Alelager |> 
  group_by(Type) |> 
  summarize(MA = mean(Alcohol), SA = sd(Alcohol), MC = mean(Calories), SC = sd(Calories), n = n())

# Quick test
t.test(Alelager$Alcohol ~ Alelager$Type) 

# Permutation test
(obs_diff <- -diff(tapply(Alelager$Alcohol, Alelager$Type, mean))) # Ale - Lager
B <- 10^4 - 1
md <- numeric(B)
for(i in 1:B){
   md[i] <- -diff(tapply(Alelager$Alcohol, sample(Alelager$Type), mean))
}
hist(md)
abline(v = obs_diff)            
pvalue <- (sum(md >= obs_diff) + sum(md <= -obs_diff) + 1)/(B + 1)
pvalue

# infer now

Alelager |> 
  specify(formula = Alcohol ~ Type) |> 
  hypothesize(null = "independence") |> 
  generate(10^4, type = "permute") |> 
  calculate(stat = "diff in means", order = c("Ale", "Lager")) -> pd
visualize(pd) + 
  shade_p_value(obs_diff, direction = "both")
get_pvalue(pd, obs_stat = obs_diff, direction = "both")
