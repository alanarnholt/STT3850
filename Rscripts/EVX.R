EVX <- function(x, px){
  EX <- sum(x*px)
  VX <- sum((x-EX)^2*px)
  SX <- sqrt(VX)
  results <- c(EX, SX, VX)
  names(results) <- c("E(X)", "S(X)", "V(X)")
  results
}

x <- 0:3
px <- c(1/8, 3/8, 3/8, 1/8)
EVX(x, px)

x <- 0:5
px <- c(1/32, 5/32, 10/32, 10/32, 5/32, 1/32)
EVX(x, px)
#####
set.seed(47)
n <- 100000
rvs <- rbinom(n, 5, 1/2)
table(rvs)/n
sd(rvs)*sqrt((n-1)/n)
var(rvs)*(n-1)/n

# Find the P(X >= 4) Given X ~ Bin(5, 1/2)
sum(dbinom(4:5, 5, 1/2))
# Or
pbinom(3, 5, 1/2, lower = FALSE)
#### Using simulation
mean(rvs >= 4)

EVX(c(0, 8, 20, 60), c(26/52, 13/52, 12/52, 1/52))
EVX(1:3, c(0.53, 0.47*.53, 0.47^2*0.53+ 0.47^3))
EVX(0:3, c(0.53, 0.47*.53, 0.47^2*0.53, 0.47^3))
EVX(c(0, 13, 26, 41), c(26/52, 13/52, 12/52, 1/52))    
EVX(1:3, c(0.46, 0.54*0.46, 0.54^2*0.46 + 0.54^3))
EVX(c(110, 110-9000, 110-2000), c(1-1/2083-1/495, 1/2083, 1/495))
