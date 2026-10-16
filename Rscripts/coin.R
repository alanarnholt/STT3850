pf31 <- dbinom(31, 50, .5)
pb31 <- dbinom(31, 50, .7)
ans <- (pf31*1/3)/(pf31*1/3 + pb31*2/3)
ans

set.seed(321)
rf31 <- mean(rbinom(100000, 50, .5)==31)
rb31 <- mean(rbinom(100000, 50, .7)==31)
SimAns <- (rf31*1/3)/(rf31*1/3 + rb31*2/3)
SimAns

