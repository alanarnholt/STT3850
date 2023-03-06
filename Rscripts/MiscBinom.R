dbinom(6, 10, 1/2)  # P(X = 6)
sum(dbinom(6:0, 10, .5))  # P(X <= 6)
choose(10, 6)*1/2^6*(1-1/2)^4  # P(X = 6)
dbinom(4, 10, .7)  # P(Y = 4 | Y~Bin(n = 10, p = .7))

xs <- rbinom(100000, 300, .03)
hist(xs)
mean(xs <= 12 & xs >=5)

z <- 0:300
pz <- dbinom(z, 300, .03)

EV <- function(x, px){
  if(sum(px) != 1)
    stop("sum of probabilities must be 1.")
  if(length(x) != length(px))
    stop("x and px must be the same length.")
  E <- sum(x*px)
  V <- sum(((x - E)^2)*px)
  S <- sqrt(V)
  c(E, V, S)
}

EV(z, pz)

# Expected Values #2
x <- 1:3
px <- c(.53, .47*.53, .47^2*.53 + .47^3)
EV(x, px)
# E(Boys)
x <- 0:3
px <- c(.53, .47*.53, .47^2*.53, .47^3)
EVX(x, px)
# SD # 1
x <- c(0, 13, 26, 41)
px <- c(26/52, 13/52, 12/52, 1/52)
EV(x, px)
# SD # 2
x <- 1:3
px <- c(.46, .54*.46, 1 - .46 - .54*.46)
EV(x, px)
# E value and variance # 1
x <- c(110 - 9000, 110 - 2000, 110)
px <- c(1/2083, 1/495, 1 -(1/2083 + 1/495))
EV(x, px)

# --------------------------------------------

  
z <- 0:300
pz <- dbinom(z, 300, .03)
EV(z, pz)
# E(Z) = 9.0, S(X) = 2.954657
# Y ~approx N(9, 2.954657)
# Solve Exactly
# P(6 <= Z <= 12) = P(Z <= 12) - P(Z <= 5)
pbinom(12, 300, 0.03) - pbinom(5, 300, 0.03)
# Or
sum(dbinom(6:12, 300, 0.03))
# Approximation now
# P(6 <= Y <= 12)
pnorm(12, 9, 2.954657) - pnorm(6, 9, 2.954657)  
  
################### 

# Suppose IQ ~ N(100, 16)

# P(84 <= IQ <= 116)  = P(IQ <= 116) - P(IQ <= 84) 
pnorm(116, 100, 16) - pnorm(84, 100, 16)

# P(IQ <= x) = 0.95
qnorm(.95, 100, 16)

#



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

