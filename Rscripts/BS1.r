# Class 10/27/2026

hours <-c(14, 15, 15, 15, 14, 13, 8, 
          17, 15, 11, 18, 18, 14, 15)
hist(hours)
# 96% CI for Mu
set.seed(123)
B <- 10000
bxbar <- numeric(B)
for(i in 1:B){
  bss <- sample(hours, size = 14, replace = TRUE)
  bxbar[i] <- mean(bss)
}
hist(bxbar)
mean(hours)
mean(bxbar)
quantile(bxbar, probs = c(0.02, 1 - 0.02))
library(infer)
library(tidyverse)
DF <- data.frame(hours)
DF

DF |> 
  specify(response = hours) |> 
  generate(reps = B, type = "bootstrap") |> 
  calculate(stat = "mean") -> BSD
BSD
ggplot(data = BSD, aes(x = stat)) +
  geom_histogram()
visualize(BSD)
BSD |> 
  summarize(LEP = quantile(stat, .02),
            UEP = quantile(stat, 1 - 0.02))
get_confidence_interval(BSD, level = .96)
get_ci(BSD, level = 0.96)
t.test(hours, conf.level = .96)$conf -> CI
CI[1]
CI[2]


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
# create bootstrap t distribution based on sample in hours
hist(hours)
(mean(hours))
(sd(hours))
sum(!is.na(hours))
set.seed(123)
B <- 100000
bts <- numeric(B)
for(i in 1:B){
  bss <- sample(hours, size = 14, replace = TRUE)
  bts[i] <- (mean(bss) - mean(hours))/(sd(bss)/sqrt(14))
}
hist(bts, freq = FALSE, breaks = "Scott")

quantile(bts, probs = c(.025, .975)) -> Q
Q
Q[1]
Q[2]
# Bootstrap t CI

c(mean(hours) -Q[2]*sd(hours)/sqrt(14), mean(hours) -Q[1]*sd(hours)/sqrt(14)) -> BTCI
BTCI

# Compare to bootstrap percentile CI
set.seed(123)
B <- 100000
bxbar <- numeric(B)
for(i in 1:B){
  bss <- sample(hours, size = 14, replace = TRUE)
  bxbar[i] <- mean(bss)
}
hist(bxbar)
mean(hours)
mean(bxbar)
quantile(bxbar, probs = c(0.025, 1 - 0.025)) -> BPCI
BPCI
# Compare to SE CI
c(mean(hours) -qt(.975, 13)*sd(bxbar), mean(hours) +qt(.975, 13)*sd(bxbar)) -> SECI
SECI
###
# Theoretical CI
t.test(hours)$conf
