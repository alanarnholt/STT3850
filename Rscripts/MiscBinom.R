dbinom(6, 10, 1/2)  # P(X = 6)
sum(dbinom(6:0, 10, .5))  # P(X <= 6)
choose(10, 6)*1/2^6*(1-1/2)^4  # P(X = 6)
dbinom(4, 10, .7)  # P(Y = 4 | Y~Bin(n = 10, p = .7))






#### Using simulation

xs <- rbinom(10000, 10, .5)
mean(xs == 6)
mean(xs <= 6)

ys <- rbinom(10000, 10, .7)
mean(ys == 4)


y <- 0:10
py <- dbinom(y, 10, .7)
DF <- data.frame(y, py)
DF
(EY <- sum(y*py))

10*.7*.3
VY <- sum((y - EY)^2*py)
VY
VY^.5

x <- 0:3
px <- c(.3, .2, .1, .4)
(EX <- sum(x*px))
(VX <- sum((x - EX)^2*px))
(SX <- VX^.5)
