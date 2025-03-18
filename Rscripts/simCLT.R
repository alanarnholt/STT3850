#
set.seed(131347)
pop <- rgamma(1000000, shape = 1, rate = 1)
hist(pop)
#
mu <- mean(pop)
sigma <- sd(pop)*(999999/1000000)
c(mu, sigma)

set.seed(44)
B <- 10000
xbar <- numeric(B)
for(i in 1:B){
  xs <- sample(pop, size = 100, replace = TRUE)
  xbar[i] <- mean(xs)
}
hist(xbar)

mean(xbar <= 0.75)
pnorm(0.75, mu, sigma/sqrt(100))
mean(xbar)
sd(xbar)
