
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
##########################################################

modpl <- lm(Sales ~ Price + ShelveLoc, data = Carseats)
summary(modpl)
library(moderndive)
library(tidyverse)
ggplot(data = Carseats, aes(x = Price, y = Sales, color = ShelveLoc)) + 
  geom_point() + 
  geom_parallel_slopes(se = FALSE)
##### Parallel Lines by hand....
ggplot(data = Carseats, aes(x = Price, y = Sales, color = ShelveLoc)) + 
  geom_point() + 
  geom_abline(slope = -0.056698, intercept = 12.001802, color = "red") + 
  geom_abline(slope = -0.056698, intercept = 12.001802 + 1.862022, color = "blue") +
  geom_abline(slope = -0.056698, intercept = 12.001802 + 4.895848, color = "darkgreen") +
  xlim(0, 200) +
  theme_bw()
####### 
ggplot(data = Carseats, aes(x = Price, y = Sales, color = ShelveLoc)) + 
  geom_point() + 
  geom_abline(slope = coef(modpl)[2], intercept = coef(modpl)[1], color = "red") + 
  geom_abline(slope = coef(modpl)[2], intercept = coef(modpl)[1] + coef(modpl)[4], color = "blue") +
  geom_abline(slope = coef(modpl)[2], intercept = coef(modpl)[1] + coef(modpl)[3], color = "darkgreen") +
  xlim(0, 200) +
  theme_bw()
#########
ggplot(data = Carseats, aes(x = Price, y = Sales, color = ShelveLoc)) + 
  geom_point() + 
  geom_abline(slope = coef(modpl)[2], intercept = coef(modpl)[1], color = "red") + 
  geom_abline(slope = coef(modpl)[2], intercept = coef(modpl)[1] + coef(modpl)[4], color = "blue") +
  geom_abline(slope = coef(modpl)[2], intercept = coef(modpl)[1] + coef(modpl)[3], color = "darkgreen") +
  xlim(0, 200) +
  theme_bw() + 
  geom_parallel_slopes()
###########
ggplot(data = Carseats, aes(x = Price, y = Sales, color = ShelveLoc)) + 
  geom_point() + 
  geom_smooth(method= "lm", se = FALSE) + 
  theme_bw()
# What model is creating the above graph? The interaction model.
modint <- lm(Sales ~ Price + ShelveLoc + Price:ShelveLoc, data = Carseats)
summary(modint)
## Graphing the model
ggplot(data = Carseats, aes(x = Price, y = Sales, color = ShelveLoc)) + 
  geom_point() + 
  geom_abline(slope = -0.055220, intercept = 11.832984, color = "red") + 
  geom_abline(slope = -0.055220 + 0.001984, intercept = 11.832984 + 1.630481, color = "blue") +
  geom_abline(slope = -0.055220 - 0.010564, intercept = 11.832984 + 6.135880, color = "darkgreen") +
  theme_bw()
#################
ggplot(data = Carseats, aes(x = Price, y = Sales, color = ShelveLoc)) + 
  geom_point() + 
  geom_abline(slope = -0.055220, intercept = 11.832984, color = "red") + 
  geom_abline(slope = -0.055220 + 0.001984, intercept = 11.832984 + 1.630481, color = "blue") +
  geom_abline(slope = -0.055220 - 0.010564, intercept = 11.832984 + 6.135880, color = "darkgreen") +
  theme_bw() + 
  geom_smooth(method = "lm")
################
ggplot(data = Carseats, aes(x = Price, y = Sales, color = ShelveLoc)) + 
  geom_point() + 
  geom_abline(slope = coef(modint)[2], intercept = 11.832984, color = "red") + 
  geom_abline(slope = coef(modint)[2] + coef(modint)[4], intercept = coef(modint)[1] + coef(modint)[4], color = "blue") +
  geom_abline(slope = coef(modint)[2] + coef(modint)[5], intercept = coef(modint)[1] + coef(modint)[3], color = "darkgreen") +
  theme_bw() + 
  geom_smooth(method = "lm")
