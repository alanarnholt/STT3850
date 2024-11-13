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




