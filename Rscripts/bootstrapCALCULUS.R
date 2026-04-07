# Bootstrap T distribution

library(PASWR2)
library(tidyverse)
CALCULUS
CALCULUS |> 
  filter(calculus == "Yes") |> 
  select(score) |> 
  pull() -> score_yes
CALCULUS |> 
  filter(calculus == "No") |> 
  select(score) |> 
  pull() -> score_no
score_yes
score_no

### One sample Bootrap T 
### (xbar* - xbar)/s*/sqrt(n)~T*

B <- 10^5
tstar <- numeric(B)
for(i in 1:B){
  bss_yes <- sample(score_yes, size = 18, replace = TRUE)
  xbarstar <- mean(bss_yes)
  sstar <- sd(bss_yes)
  tstar[i] <- (xbarstar - mean(score_yes))/(sstar/sqrt(18))
}
hist(tstar, breaks = 30)

# P(q1 <= TS <= q2) = 1-alpha.....suppose alpha = 0.10

(q1 <- quantile(tstar, probs = 0.05))
(q2 <- quantile(tstar, probs = 0.95))

# Bootstrap T CI

LL <- mean(score_yes) -q2*sd(score_yes)/sqrt(18)
UL <- mean(score_yes) -q1*sd(score_yes)/sqrt(18)
(CI <- c(LL, UL))

# Compare to thoeretical CI
t.test(score_yes, conf.level = 0.90)$conf.int

# Bootstrap T CI for mu_1 - mu_2

B <- 10^5
tstar <- numeric(B)
for(i in 1:B){
  bss_yes <- sample(score_yes, size = 18, replace = TRUE)
  bss_no <- sample(score_no, size = 18, replace = TRUE)
  tstar[i] <- ((mean(bss_no) - mean(bss_yes))-(mean(score_no) - mean(score_yes)) ) / sqrt(var(bss_no)/18 + var(bss_yes)/18)
}
hist(tstar)
(q1 <- quantile(tstar, probs = 0.05))
(q2 <- quantile(tstar, probs = 0.95))

# Bootstrap T CI for mu_1 - mu_2

LL <- (mean(score_no) - mean(score_yes)) -q2*sqrt(var(score_no)/18 + var(score_yes)/18)
UL <- (mean(score_no) - mean(score_yes)) -q1*sqrt(var(score_no)/18 + var(score_yes)/18)
c(LL, UL)
# Compare to theoretical
t.test(score ~ calculus, data = CALCULUS, conf.level = 0.90)$conf.int
