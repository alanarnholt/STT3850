# Class 1 pm 10/27/2026

hours <- c(13, 14, 16, 13, 11, 14, 14, 16, 14)
B <- 10000
bxbar <- numeric(B)
for(i in 1:B){
  bss <- sample(hours, size = 9, replace = TRUE)
  bxbar[i] <- mean(bss)
}
hist(bxbar)
quantile(bxbar, probs = c(.025, .975)) -> BPCI
BPCI

library(infer)
library(tidyverse)
TB <- tibble(hours)
TB
TB |> 
  specify(response = hours) |> 
  generate(reps = 10000, type = "bootstrap") |> 
  calculate(stat = "mean") -> BSD
BSD
visualize(BSD)
BSD |> 
  summarize(LEP = quantile(stat, 0.025),
            UEP = quantile(stat, 0.975)) -> BPCI2
BPCI2

visualize(BSD)
visualise(BSD)
get_ci(BSD, level = .95)
t.test(hours, conf.level = .95)$conf

### t-simulation
mu <- 100
sigma <- 25
n <- 25
B <- 100000
ts <- numeric(B)
for(i in 1:B){
  samp <- rnorm(n, mu, sigma)
  ts[i] <- (mean(samp) - mu)/(sd(samp)/sqrt(n))
}
hist(ts, freq = FALSE, breaks = "Scott")
curve(dt(x, n - 1), -5, 5, add = TRUE, lwd = 2)
#
#
#
### Bootstrap t-distribution
mu <- 100
sigma <- 25
n <- 25
B <- 100000
bts <- numeric(B)
samp <- rnorm(n, mu, sigma)
for(i in 1:B){
  bss <- sample(samp, n, replace = TRUE)
  bts[i] <- (mean(bss) - mean(samp))/(sd(bss)/sqrt(n))
}
hist(bts, freq = FALSE, breaks = "Scott")
curve(dt(x, n - 1), -5, 5, add = TRUE, lwd = 2)
####
set.seed(421)
hours <- round(rexp(50, .075), 0)
hours
hist(hours)

# create bootstrap t distribution based on sample in hours
hist(hours)
(mean(hours))
(sd(hours))
sum(!is.na(hours))
set.seed(123)
B <- 100000
bts <- numeric(B)
for(i in 1:B){
  bss <- sample(hours, size = 50, replace = TRUE)
  bts[i] <- (mean(bss) - mean(hours))/(sd(bss)/sqrt(50))
}
hist(bts, freq = FALSE, breaks = "Scott")

quantile(bts, probs = c(.025, .975)) -> Q
Q
Q[1]
Q[2]
# Bootstrap t CI

c(mean(hours) -Q[2]*sd(hours)/sqrt(50), mean(hours) -Q[1]*sd(hours)/sqrt(50)) -> BTCI
BTCI

# Compare to bootstrap percentile CI
set.seed(123)
B <- 100000
bxbar <- numeric(B)
for(i in 1:B){
  bss <- sample(hours, size = 50, replace = TRUE)
  bxbar[i] <- mean(bss)
}
hist(bxbar)
mean(hours)
mean(bxbar)
quantile(bxbar, probs = c(0.025, 1 - 0.025)) -> BPCI
BPCI
# Compare to SE CI
c(mean(hours) -qt(.975, 49)*sd(bxbar), mean(hours) +qt(.975, 49)*sd(bxbar)) -> SECI
SECI
###
# Theoretical CI
t.test(hours)$conf -> TCI
TCI

# Comparison of CIs
BPCI
BTCI
SECI
TCI
