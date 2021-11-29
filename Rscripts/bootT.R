# Alan Arnholt
library(resampledata)
head(Bangladesh)
Arsenic <- Bangladesh$Arsenic
Arsenic
hist(Arsenic)


B <- 10000
alpha <- 0.10
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

B <- 10000
Xbar <- numeric(B)
n <- sum(!is.na(Arsenic))
for(i in 1:B){
  bss <- sample(Arsenic, size = n, replace = TRUE)
  Xbar[i] <- mean(bss)
}
hist(Xbar)
quantile(Xbar, p = c(0.05, 0.95))



BootT1M <- function(data, level = 0.95, B = 10000){
  B = B
  alpha <- 1 - level
  tstar <- numeric(B)
  n <- sum(!is.na(data))
  xbar <- mean(data)
  for(i in 1:B){
    bss <- sample(data, size = n, replace = TRUE)
    tstar[i] <- (mean(bss) - xbar)/(sd(bss)/sqrt(n))
  }
  LT <- quantile(tstar, p = alpha/2)
  UT <- quantile(tstar, p = 1 - alpha/2)
  BT <- c(mean(data) - UT*sd(data)/sqrt(n), mean(data) - LT*sd(data)/sqrt(n))
  names(BT) <- c("Lower", "Upper")
  BT  
}
BootT1M(Arsenic, level = .90, B = 20000)


################

BootT2M <- function(vec1, vec2, level = 0.95, B = 10000){
  B = B
  alpha <- 1 - level
  tstar <- numeric(B)
  n1 <- sum(!is.na(vec1))
  n2 <- sum(!is.na(vec2))
  xbar1 <- mean(vec1)
  xbar2 <- mean(vec2)
  SE <- sqrt(var(vec1)/n1 + var(vec2)/n2)
  for(i in 1:B){
    bss1 <- sample(vec1, size = n1, replace = TRUE)
    bss2 <- sample(vec2, size = n2, replace = TRUE)
    tstar[i] <- ((mean(bss1) - mean(bss2)) - (xbar1 - xbar2))/
                          (sqrt(var(bss1)/n1 + var(bss2)/n2))
  }
  LT <- quantile(tstar, p = alpha/2)
  UT <- quantile(tstar, p = 1 - alpha/2)
  BT <- c((xbar1 - xbar2) - UT*SE, (xbar1 - xbar2) - LT*SE)
  names(BT) <- c("Lower", "Upper")
  BT  
}

library(tidyverse)
library(resampledata)
head(Verizon)
Time.ILEC <- subset(Verizon, select=Time, Group == "ILEC", drop=TRUE)
Time.CLEC <- subset(Verizon, select=Time, Group == "CLEC", drop=TRUE)

Verizon %>%
  filter(Group == "ILEC") %>%
  select(Time) -> ILEC
as.vector(ILEC$Time) -> ilec.time
ilec.time
Verizon %>%
  filter(Group == "CLEC") %>%
  select(Time) -> CLEC
as.vector(CLEC$Time) -> clec.time
BootT2M(ilec.time, clec.time)


BootT2M(Time.CLEC, Time.ILEC)








##############################

b1b0 <- function(x, y){
  r <- cor(x, y)
  b1 <- r*sd(y)/sd(x)
  b0 <- mean(y) - b1*mean(x)
  c(b0, b1)
}
set.seed(13)
x <- 1:10
y <- 1:10 + rnorm(10)
plot(x, y)



library(PASWR2)
library(ggplot2)
?CALCULUS
library(tidyverse)
CALCULUS %>%
  group_by(calculus) %>%
  summarize(MEAN = mean(score), 
            SD = sd(score), n = n())
t.test(score~calculus, data = CALCULUS, conf.level = 0.90)
calYES <- subset(CALCULUS, select=score, calculus == "Yes", drop=TRUE)
calNO <- subset(CALCULUS, select=score, calculus == "No", drop=TRUE)
t.test(score~calculus, data = CALCULUS, conf.level = 0.90)
t.test(calNO, calYES, data = CALCULUS, conf.level = 0.90)
####
library(resampledata)
Verizon
Time.ILEC <- subset(Verizon, select=Time, Group == "ILEC", drop=TRUE)
Time.CLEC <- subset(Verizon, select=Time, Group == "CLEC", drop=TRUE)
