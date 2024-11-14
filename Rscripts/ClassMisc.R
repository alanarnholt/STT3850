library(tidyverse)
library(resampledata)
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
