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

### Model building with Credit
library(ISLR)
mod.fs <- lm(Balance ~ 1, data = Credit)
SCOPE <- (~Income + Limit + Rating +Cards + Age +Education + Gender + Student + Married + Ethnicity)
add1(mod.fs, scope = SCOPE, test = "F")
#
mod.fs <- update(mod.fs, .~. + Rating)
add1(mod.fs, scope = SCOPE, test = "F")
mod.fs <- update(mod.fs, .~. + Income)
add1(mod.fs, scope = SCOPE, test = "F")
mod.fs <- update(mod.fs, .~. + Student)
add1(mod.fs, scope = SCOPE, test = "F")
mod.fs <- update(mod.fs, .~. + Limit)
add1(mod.fs, scope = SCOPE, test = "F")
mod.fs <- update(mod.fs, .~. + Cards)
add1(mod.fs, scope = SCOPE, test = "F")
mod.fs <- update(mod.fs, .~. + Age)
add1(mod.fs, scope = SCOPE, test = "F")
summary(mod.fs)
######
### Using stepAIC
library(MASS)
stepAIC(lm(Balance ~ 1, data = Credit), scope = SCOPE, direction = "forward", test = "F")
####
# OR also forward selection
null <- lm(Balance ~ 1, data = Credit)
full <- lm(Balance ~ ., data = Credit)
stepAIC(null, scope = list(lower = null, upper = full), direction = "forward", test = "F")
# Backward Elimination
mod.be <- lm(Balance ~ ., data = Credit)
drop1(mod.be, test = "F")
mod.be <- update(mod.be, .~. - Education)
drop1(mod.be, test = "F")
mod.be <- update(mod.be, .~. - Married)
drop1(mod.be, test = "F")
mod.be <- update(mod.be, .~. - Ethnicity)
drop1(mod.be, test = "F")
mod.be <- update(mod.be, .~. - ID)
drop1(mod.be, test = "F")
mod.be <- update(mod.be, .~. - Gender)
drop1(mod.be, test = "F")
summary(mod.be)
#####
## Using stepAIC
##
stepAIC(lm(Balance~., data = Credit), scope = full, direction = "backward", test = "F")
###
library(car)
residualPlots(mod.fs)
plot(mod.fs)
#####
## Using Caret
library(ISLR)
library(caret)
set.seed(3175)
trainIndex <- createDataPartition(y = Credit$Balance,
                                  p = 0.80,
                                  list = FALSE,
                                  times = 1)
training <- Credit[trainIndex, ]
testing <- Credit[-trainIndex, ]
dim(training)
mycontrol <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 5,
                          savePredictions = "final")
mod_FS <- train(y = training$Balance,
                x = training[, -c(1, 8, 9, 10, 11, 12)],
                trControl = mycontrol,
                method = "leapForward")
mod_FS
summary(mod_FS)
