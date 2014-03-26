# Problems
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
pnorm(230/800 + 1/(2*800), .286, sqrt(0.286*(1 - 0.286)/800))