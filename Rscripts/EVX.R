EVX <- function(x, px){
  stopifnot(px > 0)
  stopifnot(sum(px)==1)
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
