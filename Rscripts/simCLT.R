#
set.seed(131347)
pop <- rgamma(1000000, shape = 1, rate = 1)
hist(pop)
#
mu <- mean(pop)
sigma <- sd(pop)*(999999/1000000)
c(mu, sigma)

# Change n from 10 to 1500
set.seed(441)
n <- 100
B <- 10000
xbar <- numeric(B)
for(i in 1:B){
  xs <- sample(pop, size = n, replace = TRUE)
  xbar[i] <- mean(xs)
}
hist(xbar)

##### 
mean(xbar <= mu - 2*sigma/sqrt(n))
pnorm(mu - 2*sigma/sqrt(n), mu, sigma/sqrt(n))
## Note there is too little area in the left tail to be a normal distribution!
mean(xbar)
sd(xbar)

e1071::skewness(xbar)
