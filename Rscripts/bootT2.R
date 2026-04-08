#
library(tidyverse)
set.seed(321)
bob <- rgamma(50, 5, 0.1)
B <- 10^5
tstar <- numeric(B)
for(i in 1:B){
  bss <- sample(bob, size = 50, replace = TRUE)
  tstar[i] <- (mean(bss) - mean(bob))/(sd(bss)/sqrt(50))
}
q1 <- quantile(tstar, probs = 0.03)
q2 <- quantile(tstar, probs = 0.97)
c(q1, q2)
LL <- mean(bob) - q2*sd(bob)/sqrt(50)
UL <- mean(bob) - q1*sd(bob)/sqrt(50)
c(LL, UL)