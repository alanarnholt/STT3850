curve(dchisq(x, 1), 0, 5)
a <- qnorm(.975)
a
abline(v = a^2)
pchisq(a^2, 1)

curve(dchisq(x, 4), 0, 13)
(qchisq(.025, 4)->a)
(qchisq(.975, 4)->b)
pchisq(b, 4) - pchisq(a, 4)

qnorm(0.10, 88, 4) -> a
a
qnorm(c(0.10, 0.25, 0.75, 0.90), 88, 4)

qnorm(c(0.10, 0.25, 0.75, 0.90), 30, 5)
# P(gbar >= 90 | N(88, 1))
1 - pnorm(90, 88, 1)
pnorm(90, 88, 1, lower = FALSE)






qnorm(c(.1,.25,.75, .90), 31, 4)



set.seed(34)
stuff <- rgamma(50, 1, 1)
hist(stuff)
xbar <- mean(stuff)
SD <- sd(stuff)
SD/xbar
set.seed(551)
B <- 10^4
cv <- numeric(B)
for(i in 1:B){
  bss <- sample(stuff, size = 50, replace = TRUE)
  cv[i] <- sd(bss)/mean(bss)
}
hist(cv)
quantile(cv, probs = c(0.025, 0.975)) -> CI
CI

BootBias <- mean(cv) - SD/xbar
BootBias