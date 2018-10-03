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
#####
mod2 <- lm(medv ~ rm + lstat, data = Boston)
summary(mod2)
X <- model.matrix(mod2)
y <- Boston$medv
betahat <- solve(t(X)%*%X)%*%t(X)%*%y
betahat
XTXI <- summary(mod2)$cov.unscaled
MSE <- summary(mod2)$sigma^2
var.cov.b <- MSE*XTXI
var.cov.b
diag(var.cov.b)^.5
# or vcov
vcov(mod2)
diag(vcov(mod2))^.5
betahat/diag(vcov(mod2))^.5

plot(mod2)
par(mfrow = c(2, 2))
plot(mod2)
par(mfrow = c(1, 1))

#####
library(rpart)
model <- rpart(medv~., data = Boston)
model
library(rattle)
fancyRpartPlot(model)
library(partykit)
mod3 <- ctree(medv ~., data = Boston)
plot(mod3)

####
library(dplyr)
library(ggplot2)
library(ISLR)
ggplot(data = Credit, aes(x = Rating, y = Balance, color = Gender)) + 
  geom_point() +
  theme_bw()

ggplot(data = Credit, aes(x = Rating, y = Balance, color = Ethnicity)) + 
  geom_point() +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE)

Credit %>% 
  group_by(Ethnicity) %>% 
  summarize(Mean = mean(Balance))

TG <- Credit %>% 
  group_by(Gender) %>% 
  summarize(Mean = mean(Balance))
TG


mod <- lm(Balance ~ Ethnicity, data = Credit)
summary(mod)
mod <- lm(Balance ~ Gender, data = Credit)
summary(mod)
#
mod32 <- lm(Balance ~ Ethnicity + Rating, data = Credit)
summary(mod32)
#
mod33 <- lm(Balance ~ Gender + Rating, data = Credit)
summary(mod33)
####
library(carData)
str(Prestige)
ggplot(data = na.omit(Prestige), aes(x = prestige, y = income, color = type)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()
####
Prestige %>% 
  na.omit() %>% 
  group_by(type) %>% 
  summarize(Mean = mean(income))


mod <- lm(income ~ type, data = Prestige)
summary(mod)
summary(mod)$coeff[1, 1]

ggplot(data = na.omit(Prestige), aes(x = prestige, y = income, color = type)) +
  geom_point() +
  theme_bw() + 
  geom_hline(yintercept = c(summary(mod)$coeff[1, 1], 
                            summary(mod)$coeff[1, 1] + summary(mod)$coeff[2, 1], 
                            summary(mod)$coeff[1, 1] + summary(mod)$coeff[3, 1]))
CP <- na.omit(Prestige)
y <- CP$income
X <- model.matrix(mod)
head(X)
solve(t(X)%*%X)
betahat <- solve(t(X)%*%X)%*%t(X)%*%y
betahat
###
contrasts(Prestige$type)
