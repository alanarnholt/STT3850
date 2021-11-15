# Alan Arnholt
library(resampledata)
head(Bangladesh)
Arsenic <- Bangladesh$Arsenic
Arsenic
hist(Arsenic)


B <- 10000
alpha <- 0.05
tstar <- numeric(B)
xbar <- mean(Arsenic)
n <- sum(!is.na(Arsenic))
for(i in 1:B){
  bss <- sample(Arsenic, size = n, replace = TRUE)
  tstar[i] <- (mean(bss) - xbar)/(sd(bss)/sqrt(n))
}
hist(tstar)
mean(tstar)
sd(tstar)
LT <- quantile(tstar, p = alpha/2)
UT <- quantile(tstar,  p = 1 - alpha/2)
c(LT, UT)
BT <- c(xbar - UT*sd(Arsenic)/sqrt(n), xbar - LT*sd(Arsenic)/sqrt(n))
names(BT) <- c("Lower", "Upper")
BT



BootT <- function(data, level = 0.95, B = 10000){
  B = B
  alpha <- 1 - level
  tstar <- numeric(B)
  xbar <- mean(data)
  n <- sum(!is.na(data))
  for(i in 1:B){
    bss <- sample(data, size = n, replace = TRUE)
    tstar[i] <- (mean(bss) - xbar)/(sd(bss)/sqrt(n))
  }
  LT <- quantile(tstar, p = alpha/2)
  UT <- quantile(tstar, p = 1 - alpha/2)
  BT <- c(xbar - UT*sd(data)/sqrt(n), xbar - LT*sd(data)/sqrt(n))
  names(BT) <- c("Lower", "Upper")
  BT  
}
BootT(Arsenic, level = .90, B = 20000)

