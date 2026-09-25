#1 .
Credit <- read.csv("http://statlearning.com/s/Credit.csv")
head(Credit)
library(tidyverse)
library(moderndive)
#2. 
mod_num <- lm(Balance ~ Income + Limit + Rating + Cards + Age, data = Credit)
#3. 
model.matrix(mod_num) -> X
X
#4. 
XTX <- t(X)%*%X
XTX
#5. 
XTXI <- solve(XTX)
XTXI
# 6. 
summary(mod_num)$cov.unscaled
#7.
betahat <- solve(t(X)%*%X)%*%t(X)%*%Credit$Balance
betahat
coef(mod_num)
#8.
MSE <- summary(mod_num)$sigma^2
MSE
#9.
var_cov_b <- MSE*XTXI
var_cov_b
#10.
diag(var_cov_b)
se_b <- diag(var_cov_b)^.5
se_b
b <- coef(mod_num)
b
se_b
#11.
Ts <- b/se_b
Ts
#12.
ps <- pt(abs(Ts), 394, lower = FALSE)*2
ps
summary(mod_num)
# Confidence Intervals 
c(b - qt(.975, 394)*se_b, b + qt(.975, 394)*se_b)
confint(mod_num)
get_regression_table(mod_num)
