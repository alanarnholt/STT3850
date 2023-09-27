
library(ISLR)
library(ISLR2)
null <- lm(Balance ~ 1, data = Credit)
full <- lm(Balance ~ ., data = Credit)
library(MASS)
modC <- stepAIC(full, scope = list(lower = null, upper = full), direction = "backward", test = "F")
modC
modD <- stepAIC(null, scope = list(lower = null, upper = full), direction = "forward", test = "F")
modD
modC
##################
null <- lm(Sales ~ 1, data = Carseats)
summary(null)
full <- lm(Sales ~ ., data = Carseats)
summary(full)
modBACK <- stepAIC(full, scope = list(lower = null, upper = full), direction = "backward", test = "F")
modBACK
modFORW <- stepAIC(null, scope = list(lower = null, upper = full), direction = "forward", test = "F")
modFORW
modBACK
