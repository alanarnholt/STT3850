# Probability Problems

A <- .20*.01
B <- .40*.01
C <- .40*.05

C/(A+B+C)

.45*.75/(.45*.75 + .25*.15)

x <- c(0, 8, 20, 60)
px <- c(26/52, 13/52, 12/52, 1/52)

EX <- sum(x*px)
EX

x <- c(1, 2, 3)
px <- c(.53, .47*.53, .47^3 + .47^2*.53)
EX <- sum(x*px)
EX
y <- c(0, 1, 2, 3)
py <- c(.53, .47*.53, .47^2*.53, .47^3)
EY <- sum(y*py)
EY

x <- c(0, 13, 26, 26+15)
px <- c(26/52, 13/52, 12/52, 1/52)
EX <- sum(x*px)
VX <- sum((x - EX)^2*px)
SX <- sqrt(VX)
SX

x <- c(1, 2, 3)
px <- c(.46, .46*.54, .54^2*.46+.54^3)
EX <- sum(x*px)
EX
VX <- sum((x - EX)^2*px)
SX <- sqrt(VX)
SX

x <- c(110, 110-9000, 110-2000)
px <- c(1 - 1/2083 - 1/495, 1/2083, 1/495)
EX <- sum(x*px)
EX
VX <- sum((x - EX)^2*px)
SX <- sqrt(VX)
SX


# C ~N(320, 22) D ~N(160, 16)
# P(6*C > 2000)= ?
# 6*C ~ N(6*320, sqrt(6*22^2) )
# 6*C ~ N(1920, 53.88877)
# P(6C >= 2000) = 1- P(6C <= 2000)
1 - pnorm(2000, 1920, sqrt(6*22^2))


# .50C + .4D ~ how???
# Z = .50C + .4D ~N(.5*320 + .4*160, sqrt(.5^2*22^2 + .4^2*16^2) )
# Z ~N(224, 12.72635)
pnorm(300, 224, 12.72, lower = FALSE)

# Probability he will sell a doughnut to more than half of his coffee customers?
# P(D - C/2 > 0) 
# Z = D - C/2 ~ N(160 - 160 = 0, ).....so 1/2


# P(275 <= PREG <= 290)=?
pnorm(290, 265, 15) - pnorm(275, 265, 15)
# P(Preg <= ???) = 0.80
qnorm(.80, 265, 15)

