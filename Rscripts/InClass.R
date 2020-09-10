# Author: Alan T. Arnholt
# Date: 9/8/20

library(moderndive)
library(tidyverse)
library(PASWR2)

### 

glimpse(VIT2005)

VIT2005 <- VIT2005 %>% 
  mutate(elevatorF = as.factor(elevator)) %>% 
  glimpse()

##
# Exercise: 1) Create a scatterplot of totalprice versus
# area; 2) Compute r using the formula (1.1 from yesterday) 
# and verify your answer using the built-in R function cor(); 
# 3) Create a linear model object named mod1 by regressing 
# totalprice on to area; 4) Write the least squares
# regression equation; 5) Using the coefficients from 4)
# use mutate to compute the yhat and residual values.
#
###
ggplot(data = VIT2005, aes(x = area, y = totalprice)) +
  geom_point() + 
  theme_bw()
###
### r = sum( ((x - xbar)/s_x) * ((y - ybar)/s_y))
values <- VIT2005 %>% 
  mutate(x_xbar = area - mean(area), y_ybar = totalprice - mean(totalprice),
         zx = x_xbar/sd(area), zy  = y_ybar/sd(totalprice)) %>% 
  select(area, x_xbar, zx, totalprice, y_ybar, zy)
head(values)
values %>% 
  summarize(r = sum(zx*zy)/(length(zx) - 1))
### 
#  Built In Function
VIT2005 %>% 
  summarize(r = cor(totalprice, area))
###
# Model 1: mod1
###
mod1 <- lm(totalprice ~ area, data = VIT2005)
mod1
###
# LS Model: yhat = bo + b1*x
summary(mod1)
###
### yhat = 40822.416 + 2704.751*x
values <- values %>% 
  mutate(yhat = 40822.416 + 2704.751*area,
         ei = totalprice -yhat) %>% 
  select(x = area, y = totalprice, yhat, ei)
head(values)
######################
## I go over the next part
######################
names(summary(mod1))
### Solution 1
coef(summary(mod1))
### Solution 2
### Wrapper Functions from ModernDive
###
get_regression_table(mod1)
###
### Solution 3 use broom
library(broom)
tidy(mod1)
###
anova(mod1)
###
### Residuals: e_i = y_i - yhat_i
###
values <- values %>% 
  mutate(e = y - yhat)
head(values)
###
# Obtaining the fitted and residuals...
# But, they are returned as vectors...not data frames or tibbles
fitted(mod1)
resid(mod1)
### Solution 1
stuff <- VIT2005 %>% 
  mutate(yhat = fitted(mod1), e = resid(mod1))
head(stuff)
### Solution 2
library(broom)
MUCHO <- augment(mod1)
head(MUCHO)
###
### Solution 3
### Wrapper Functions from ModernDive
stuff2 <- get_regression_points(mod1)
head(stuff2)
###
###  b0 = beta_0_hat; b1 = beta_1_hat
### b1 = r*sy/sx; b0 = ybar - b1*xbar
###
VIT2005 %>% 
  summarize(r = cor(totalprice, area), 
            b1 = r*sd(totalprice)/sd(area), 
            b0 = mean(totalprice) - b1*mean(area))
### Wrapper Now
get_regression_table(mod1)
# OR
tidy(mod1)
###############################################################
# ggplot(data = VIT2005, aes(x = elevatorF, totalprice)) + 
#   geom_point() + 
#   theme_bw()
###############################################################
### Prediction with categorical variable: elevator

####
# Exercise: 1) Create a linear model object mod2 by 
# regressing totalprice on to elevatorF; 2) What
# totalprice do you predict for an apartment
# without an elevator and with an elevator 
# (use the output from mod2 only); 3) Answer 2)
# by using group_by() and mean

mod2 <- lm(totalprice ~ elevatorF, data = VIT2005)
mod2

## Price of flat without elevator: 210492
## Price of flat with elevator: 210492 + 88014 = 298506

VIT2005 %>% 
  group_by(elevatorF) %>% 
  summarize(Avg = mean(totalprice))

#####################################################
#
# Me talk through the next part
#

# Model 2
mod2 <- lm(totalprice ~ area + elevatorF + area:elevatorF, data = VIT2005)
mod2
# LS fit for elevator: (113114 - 50872) + (1344 + 1202)*area
# LS fit for no elevator: 113114 + 1344*area
summary(mod2)
####
### Fit a parallel slopes model
####
mod3 <- lm(totalprice ~ area + elevatorF, data = VIT2005)
mod3
summary(mod3)
get_regression_table(mod3)
# Or
tidy(mod3)
# LS line with elevator: (36174 + 39091) + 2405*area
# LS line without elevator: 36174 + 2405*area
### Draw a parallel slopes model
ggplot(data = VIT2005, aes(x = area, y = totalprice, color = elevatorF)) +
  geom_point() + 
  geom_parallel_slopes(se = FALSE) + 
  theme_bw()
###
## Create plot manually
ggplot(data = VIT2005, aes(x = area, y = totalprice, color = elevatorF)) + 
  geom_point() + 
  geom_abline(intercept = (36174 + 39091), slope = 2405, color = "blue") + 
  geom_abline(intercept = 36174, slope = 2405, color = "red") + 
  scale_color_manual(values = c("red", "blue")) + 
  theme_bw()
######################################################
### Exercise: 1) Create a parallel slopes model by 
### regressing score on byt_avg and rank.  Store the result
### in mod4; 2) Write out the LS lines for the three ranks;
### 3) Graph mod4 with ggplot (parallel slopes model)

mod4 <- lm(score ~ bty_avg + rank, data = evals)
mod4

# Write out the LS lines for the three ranks.

## LS line for teaching: (398155) + 0.06783*bty_avg
## LS line for tenure track: (398155 - 0.16070) + 0.06783*bty_avg
## LS line for tenured: (398155 - 0.12623) _ 0.06783*bty_avg

## Graph mod4 with ggplot

ggplot(data = evals, aes(x = bty_avg, y = score, color = rank)) + 
  geom_point() + 
  geom_parallel_slopes(se = FALSE) + 
  theme_bw()
####
get_regression_table(mod4)
summary(mod4)
tidy(mod4)
##############################################
#
#  Me talk through the next part
#
### Create mod5 by regressing score on bty_avg
mod5 <- lm(score ~ bty_avg, data = evals)
mod5

### add mod5 to the parallel slopes graph of mod4
ggplot(data = evals, aes(x = bty_avg, y = score, color = rank)) + 
  geom_point() + 
  geom_parallel_slopes(se = FALSE) + 
  geom_abline(intercept = 3.88034, slope = 0.0644, color = "gray", lty = "dashed") +
  theme_bw()
#################
rp <- get_regression_points(mod4)
head(rp)
# Or 
rp2 <- augment(mod4)
head(rp2)
########
ggplot(data = rp, aes(x = residual)) + 
  geom_histogram(binwidth = 0.25, fill = "pink", color = "black") + 
  theme_bw()

ggplot(data = rp, aes(sample = residual)) + 
  geom_qq(size = 1, color = "lightblue") + 
  geom_qq_line(color = "red", linetype = "dashed") +
  theme_bw()
######
library(ggfortify)
autoplot(mod4, which = 2, nrow = 1, ncol = 1) + theme_bw()
autoplot(mod4, which = 1:6, nrow = 2, ncol = 3) + theme_bw()
autoplot(mod4, which = 2:1, nrow = 1, ncol = 2) + theme_bw()
#####