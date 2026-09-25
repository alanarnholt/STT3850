Credit <- read.csv("http://statlearning.com/s/Credit.csv")
head(Credit)
library(tidyverse)
library(moderndive)
mod_num <- lm(Balance ~ Income + Limit + Rating + Cards + Age, data = Credit)
model.matrix(mod_num) -> X
head(X)
betahat <- solve(t(X)%*%X)%*%t(X)%*%Credit$Balance
betahat
coef(mod_num)
XTXI <- summary(mod_num)$cov.unscaled
XTXI
MSE <- summary(mod_num)$sigma^2
MSE
var_cov_b <- MSE*XTXI
var_cov_b
diag(var_cov_b)
se_b <- diag(var_cov_b)^.5
b <- coef(mod_num)
b
se_b
Ts <- b/se_b
Ts
ps <- pt(abs(Ts), 394, lower = FALSE)*2
ps
summary(mod_num)
 
c(b - qt(.975, 394)*se_b, b + qt(.975, 394)*se_b)
confint(mod_num)
get_regression_table(mod_num)
