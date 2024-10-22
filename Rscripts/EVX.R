EVX <- function(x, px){
  EX <- sum(x*px)
  VX <- sum((x-EX)^2*px)
  SX <- sqrt(VX)
  c(EX, SX, VX)
}
x <- 0:3
px <- c(1/8, 3/8, 3/8, 1/8)
EVX(x, px)