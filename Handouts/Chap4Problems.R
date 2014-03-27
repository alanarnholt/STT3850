# Problems
## 1
Population <- c(1, 2, 5, 6, 10, 12)
SS <- t(combn(x = Population, m = 3))
MD <- apply(SS, 1, median)
SSS <- cbind(SS, MD)
library(ggplot2)
ggplot(data = data.frame(x = MD), aes(x = x)) + geom_density() + theme_bw()
median(Population)
mean(MD)
# 2
Population <- c(3, 6, 7, 9, 11, 14)
MIN <- apply(combn(Population, 3), 2, min)
MIN
mean(MIN)  # mean of the sampling distribution
# This statistic is an estimate of the population Min (3).
ggplot(data = data.frame(x = MIN), aes(x = x)) + geom_density() + theme_bw()
#

####
A <- c(1, 3, 4, 5)
B <- c(5, 7, 9)
SS <- expand.grid(A = A, B = B)
SS
XPY <- apply(SS, 1, sum)
XPY  # Distribution of X + Y
# b. Sampling with and without with a sample of size one is the same....
mean(A)
mean(B)
mean(XPY)
# First two add to the last one.
mean(XPY >= 13)
# 4
f <- function(x){2/x^2}
curve(f, 1, 2)
integrate(f, 1, 2)$value
# P(W < 1.5)
integrate(f, 1, 1.5)$value
# 5
f <- function(x){3*x^2/8}
integrate(f, 0, 2)$value
integrate(f, 0, 1/5)$value
# 6 X ~(48, 9)...n = 30...Xbar approx~N(48, 9/sqrt(30)).  P(Xbar > 51)
pnorm(51, 48, 9/sqrt(30), lower = FALSE)
#
# 7 phat ~ approx N(0.55, sqrt(0.55*0.45/36))
# P(phat <= 0.50)
pnorm(.5 + 1/(2*36), .55, sqrt(.55*.45/36))
#
# 8 
pnorm(4.6, 6, sqrt(10/20))
# 9 
f <- function(x){(3/16)*(x - 4)^2}
integrate(f, 2, 6)$value
xfx <- function(x){x*(3/16)*(x - 4)^2}
EX <- integrate(xfx, 2, 6)$value
x2fx <- function(x) {x^2*(3/16)*(x - 4)^2}
EX2 <- integrate(x2fx, 2, 6)$value
VX <- EX2 - EX^2
VX
# P(Xbar >= 4.2) = 0.02186875
pnorm(4.2, EX, sqrt(VX)/sqrt(244), lower = FALSE)
## 10
## phat ~approx N(.286, sqrt(0.286*(1 - 0.286)/800) )
## P(220/800 < phat < 230/800) = 
pnorm(230/800 + 1/(2*800), .286, sqrt(0.286*(1 - 0.286)/800)) - 
  pnorm(220/800 - 1/(2*800), .286, sqrt(0.286*(1 - 0.286)/800))
## Exact answer
sum(dbinom(220:230, 800, 0.286))
## or
pbinom(230, 800, .286) - pbinom(219, 800, .286)
## I like this type of problem....
## 11 xbar ~ N(1/2, 1/sqrt(12n)).....P(|xbar - 1/2|/(1/sqrt(12n)) < 1.6448) >= 0.90
##
n <- ceiling((qnorm(.95)/.05)^2/12)
n
## 12
# E[X] = 1/lambda = 1/(1/10) = 10
set.seed(13)
sims <- 10000
xbar <- numeric(sims)
for(i in 1:sims){
  xbar[i] <- mean(rexp(30, 1/10))
}
mean(xbar)
mean(xbar >= 12)
library(ggplot2)
ggplot(data = data.frame(x = xbar), aes(x = x)) + geom_density() + theme_bw() + stat_function(fun = dgamma, args = list(30*1, 30/10), lty = "dashed", color = "red")
##
## 13
## W ~ N(36, sqrt(8^2/10 + 7^2/15) = 3.109126)
set.seed(13)
sims <- 10000
xbar <- numeric(sims)
ybar <- numeric(sims)
for(i in 1:sims){
  xbar[i] <- mean(rnorm(10, 20, 8))
  ybar[i] <- mean(rnorm(15, 16, 7))
}
W <- xbar + ybar
mean(W)  # close to 36
sd(W)    # close to 3.11
mean(W < 40)
# Exact answer
pnorm(40, 36, sqrt(8^2/10 + 7^2/15))
#####
## 14
# W ~ N(-3, sqrt(3^2/9 + 5^2/12) = 1.755942)
set.seed(13)
sims <- 10000
xbar <- numeric(sims)
ybar <- numeric(sims)
for(i in 1:sims){
  xbar[i] <- mean(rnorm(9, 7, 3))
  ybar[i] <- mean(rnorm(12, 10, 5))
}
W <- xbar - ybar
mean(W)  # close to -3
sd(W)    # close to 1.76
mean(W < 1.5)
# Exact answer
pnorm(1.5, -3, sqrt(3^2/9 + 5^2/12))
## 15 & 16

## 17
set.seed(13)
sims <- 10000
WE <- numeric(sims)
for(i in 1:sims){
  WE[i] <- sum(rexp(20, 2))
}
mean(WE)
var(WE)
mean(WE <= 10)
## 18
set.seed(13)
sims <- 10000
WE <- numeric(sims)
for(i in 1:sims){
  WE[i] <- mean(rexp(30, 1/3))
}
mean(WE)
sd(WE)
mean(WE <= 3.5)
## 19
curve(dexp(x, 1/20), 0, 100)
abline(v = qexp(0.5, 1/20))
abline(h = 0)
qexp(0.5, 1/20)
##
set.seed(13)
first <- rexp(50, 1/20)
mean(first)
sd(first)
#
sims <- 10000
MD50 <- numeric(sims)
MD100 <- numeric(sims)
MD500 <- numeric(sims)
MD1000 <- numeric(sims)
for(i in 1:sims){
  MD50[i] <- median(rexp(50, 1/20))
  MD100[i] <- median(rexp(100, 1/20))
  MD500[i] <- median(rexp(500, 1/20))
  MD1000[i] <- median(rexp(1000, 1/20))
}
c(mean(MD50), mean(MD100), mean(MD500), mean(MD1000), sd(MD50), sd(MD100), sd(MD500), sd(MD1000))
## 20

