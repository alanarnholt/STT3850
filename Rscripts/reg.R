# Regression

library(MASS)
str(Boston)
library(dplyr)
glimpse(Boston)
library(skimr)
skim(Boston)
cor(Boston)
#
plot(medv ~ rm, data = Boston, col = "red")
plot(medv ~ lstat, data = Boston, col = "blue")
plot(medv ~ I(lstat^0.5), data = Boston, col = "blue")
##
library(ggplot2)
ggplot(data = Boston, aes(x = lstat, y = medv)) +
  geom_point() + 
  theme_bw()
##
ggplot(data = Boston, aes(x = lstat, y = medv)) +
  geom_point(color = "purple") + 
  scale_x_sqrt() +
  theme_bw() + 
  geom_smooth(method = "lm")
#
mod <- lm(medv ~ I(lstat^0.5), data = Boston)
summary(mod)
#
library(moderndive)
get_regression_table(mod)
# Note get_regression_points() does not work on mod
# get_regression_points(mod)
mod2 <- lm(medv ~ lstat, data = Boston)
reg_points <- get_regression_points(mod2)
reg_points
##
ggplot(data = Boston, aes(x = lstat, y = resid(mod))) + 
  geom_point() + 
  theme_bw()
summary(mod)
