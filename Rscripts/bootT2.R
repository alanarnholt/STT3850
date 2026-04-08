#
library(tidyverse)
set.seed(439)
tristan <- rgamma(50, shape = 5, rate = 0.1)
B <- 10^5
tstar <- numeric(B)
for(i in 1:B){
  bss <- sample(tristan, size = 50, replace = TRUE)
  tstar[i] <- (mean(bss) - mean(tristan))/(sd(bss)/sqrt(50))
}
q1 <- quantile(tstar, probs = 0.03)
q2 <- quantile(tstar, probs = 0.97)
c(q1, q2)
LL <- mean(tristan) - q2*sd(tristan)/sqrt(50)
UL <- mean(tristan) - q1*sd(tristan)/sqrt(50)
c(LL, UL)
t.test(tristan, conf.level = 0.94)$conf.int

pnorm(1.5) - pnorm(-1.5)
qt(.90, 10)
