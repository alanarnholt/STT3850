# Alan Arnholt
# 10/26/2023
library(tidyverse)
library(infer)
library(resampledata)
#
births <- NCBirths2004
head(births)
names(births)
# Select only Female babies
births %>% 
  filter(Gender == "Female") -> birthsF
head(birthsF)
# extract weights
birthsF$Weight -> Fweight
head(Fweight)
#
hist(Fweight)
qqnorm(Fweight)
qqline(Fweight)
# Computing values for CI
xbar <- mean(Fweight)
S <- sd(Fweight)
n <- length(Fweight)
CT <- qt(.95, n-1)
CI <- xbar + c(-1, 1)*CT*S/sqrt(n)
CI # 90% CI for mu
# Using t.test()
t.test(Fweight, conf.level = 0.90)
# Or
t.test(Fweight, conf.level = 0.90)$conf
CIT <- t.test(Fweight, conf.level = 0.90)$conf
CIT
CIT[1]
CIT[2]
###############################################
# Bootstrap Percentile CI for mu
B <- 10000
bstat <- numeric(B)
for(i in 1:B){
  bss <- sample(Fweight, size = sum(!is.na(Fweight)), replace = TRUE)
  bstat[i] <- mean(bss)
}
hist(bstat)
BPCI <- quantile(bstat, probs = c(0.05, 0.95))
BPCI
#### Using the infer pipeline now
birthsF %>% 
  specify(response = Weight) %>% 
  generate(reps = 10000, type = "bootstrap") %>% 
  calculate(stat = "mean") -> IBPV
# using the wrapper function
get_ci(IBPV, level = 0.90) -> IBPCI
IBPCI
# or
quantile(IBPV$stat, probs = c(0.05, 0.95))

#################################################
## Bootstrap SE method
xbar <- mean(Fweight)
SExbarstar <- sd(bstat)
CT <- qt(.95, n-1)
CT
BSECI <- xbar + c(-1, 1)*CT*SExbarstar
BSECI
## using infer now
get_ci(IBPV, level = 0.90, type = "se", point_estimate = xbar)
#################################################
## Bootstrap t method----2nd order accurate!
B <- 10000
bt <- numeric(B)
xbarstar <- numeric(B)
for(i in 1:B){
  bss <- sample(Fweight, size = sum(!is.na(Fweight)), replace = TRUE)
  xbarstar[i] <- mean(bss)
  bt[i] <- (mean(bss) - mean(Fweight))/(sd(bss)/sqrt(n))
}
BT <- (xbarstar - mean(Fweight))/sd(xbarstar)
QT <- quantile(BT, probs = c(0.05, 0.95))  
QT
Q <- quantile(bt, probs = c(0.05, 0.95))
Q
BSTCI_1 <- c(xbar - Q[2]*sd(Fweight)/sqrt(n),
             xbar - Q[1]*sd(Fweight)/sqrt(n))
BSTCI_1
BSTCI_2 <- c(xbar - QT[2]*sd(Fweight)/sqrt(n),
             xbar - QT[1]*sd(Fweight)/sqrt(n))
BSTCI_2
#################################################
## Comparing the answers of the 4 CIs
## Formula
CIT
## Bootstrap Percentile
BPCI
## Bootstrap Standard Error
BSECI
## Bootstrap t*
BSTCI_1
####################################
## CV = sigma/mu
## Construct a bootstrap percentile CI for the CV of Female weight - 
## this is a statistic not available in infer
CVhat = S/xbar
CVhat
######
B <- 10000
CVstar <- numeric(B)
for(i in 1:B){
  bss <- sample(Fweight, size = sum(!is.na(Fweight)), replace = TRUE)
  CVstar[i] <- sd(bss)/mean(bss)
}
BT <- (CVstar - sd(Fweight)/mean(Fweight)) / sd(CVstar)
QT <- quantile(BT, probs = c(0.05, 0.95))
QT
hist(CVstar)
# 90% CI for CV
quantile(CVstar, probs = c(0.05, 0.95))
# 90% BTCI
c(sd(Fweight)/mean(Fweight) - QT[2]*sd(CVstar), 
  sd(Fweight)/mean(Fweight) - QT[1]*sd(CVstar))

####################################
## Construct a bootstrap percentile CI for the ratio of 
## Female variance over Male variance
births %>% 
  filter(Gender == "Female") -> birthsF
# extract weights
birthsF$Weight -> Fweight
births %>% 
  filter(Gender == "Male") -> birthsM
# extract weights
birthsM$Weight -> Mweight

B <- 10000
VFVMstar <- numeric(B)
for(i in 1:B){
  bssF <- sample(Fweight, size = sum(!is.na(Fweight)), replace = TRUE)
  bssM <- sample(Mweight, size = sum(!is.na(Mweight)), replace = TRUE)
  VFVMstar[i] <- var(bssF)/var(bssM)
}
###
hist(VFVMstar)
quantile(VFVMstar, probs = c(0.05, 0.95)) # BPCI
#####
## BSECI----this one is a stretch for the class?
## stat -+ t_{1-alpha/2, df}*\hat{SE}_B{stat}
var(Fweight)/var(Mweight) +c(-1,1)*qt(.95, 487)*sd(VFVMstar)
#####
