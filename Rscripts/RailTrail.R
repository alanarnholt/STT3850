# RailTrail
library(tidyverse)
library(mosaicData)
?RailTrail

ggplot(data = RailTrail, aes(x = hightemp, y= volume)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

library(moderndive)
mod1 <- lm(volume ~ hightemp, data = RailTrail)
summary(mod1)
get_regression_table(mod1)

ggplot(data = RailTrail, aes(x = hightemp, y = volume, color  = weekday)) + 
  geom_point() + 
  theme_bw()
mod2 <- lm(volume ~ hightemp + weekday, data = RailTrail)
summary(mod2)

ggplot(data = RailTrail, aes(x = hightemp, y = volume, color  = weekday)) + 
  geom_point() + 
  theme_bw() + 
  geom_parallel_slopes(se = FALSE) + 
  labs(color = "Is it a\nweekday?") + 
  geom_abline(intercept = coef(mod2)[1], slope = coef(mod2)[2], color = "red", size = 0.25) + 
  geom_abline(intercept = coef(mod2)[1] + coef(mod2)[3], slope = coef(mod2)[2], color = "lightblue", size = 0.25) 


################################################


nullmodel <- lm(volume ~ 1, data = RailTrail)
fullmodel <- lm(volume ~ ., data = RailTrail)
summary(nullmodel)
summary(fullmodel)
library(MASS)
stepAIC(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), 
        direction = "forward", test = "F")
stepAIC(fullmodel, scope = list(lower = nullmodel, upper = fullmodel), 
        direction = "backward", test = "F")


air <- na.omit(airquality)
nullmodel <- lm(Ozone ~ 1, data = air)
fullmodel <- lm(Ozone ~ ., data = air)
summary(nullmodel)
summary(fullmodel)
library(MASS)
stepAIC(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "forward", test = "F")
stepAIC(fullmodel, scope = list(lower = nullmodel, upper = fullmodel), 
        direction = "backward", test = "F")


# Imputing values
library(simputation)
imp_air <- airquality %>% 
  impute_lm(Ozone ~ Solar.R + Wind + Temp + Month) %>% 
  impute_lm(Solar.R ~ Wind + Temp + Month) %>% 
  impute_lm(Ozone ~ Solar.R + Wind + Temp + Month)
summary(imp_air)
nullmodel <- lm(Ozone ~ 1, data = imp_air)
fullmodel <- lm(Ozone ~ ., data = imp_air)
summary(nullmodel)
summary(fullmodel)
library(MASS)
stepAIC(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), 
        direction = "forward", test = "F")
stepAIC(fullmodel, scope = list(lower = nullmodel, upper = fullmodel), 
        direction = "backward", test = "F")


library(ISLR)
summary(Credit)
nullmodel <- lm(Balance ~ 1, data = Credit)
fullmodel <- lm(Balance ~ ., data = Credit)
summary(nullmodel)
summary(fullmodel)
library(MASS)
stepAIC(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "forward", test = "F")
stepAIC(fullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "backward", test = "F")

nullmodel <- lm(Income ~ 1, data = Credit)
fullmodel <- lm(Income ~ ., data = Credit)
summary(nullmodel)
summary(fullmodel)
library(MASS)
stepAIC(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "forward", test = "F")
stepAIC(fullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "backward", test = "F")


nullmodel <- lm(Rating ~ 1, data = Credit)
fullmodel <- lm(Rating ~ ., data = Credit)
summary(nullmodel)
summary(fullmodel)
library(MASS)
stepAIC(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "forward", test = "F")
stepAIC(fullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "backward", test = "F")


nullmodel <- lm(Rating ~ 1, data = Credit)
fullmodel <- lm(Rating ~ ., data = Credit)
summary(nullmodel)
summary(fullmodel)
library(MASS)
stepAIC(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "forward", test = "F")
stepAIC(fullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "backward", test = "F")
##########################
library(BSDA)
?Simpson

ggplot(data = Simpson, aes(x = gpa, fill = gender)) +
  geom_histogram(binwidth = 0.1) +
  facet_grid(vars(gender)) + 
  theme_bw()

Simpson %>% 
  group_by(gender) %>% 
  summarize(M = median(gpa), n = n())

ggplot(data = Simpson, aes(x = gender, y = gpa, fill = gender)) + 
  geom_boxplot() + 
  theme_bw()

Simpson %>% 
  group_by(sport, gender) %>% 
  summarize(M = median(gpa), n = n())

##
ggplot(data = Simpson, aes(x = gender, y = gpa, fill = gender)) +
  geom_boxplot() + 
  facet_grid(cols = vars(sport)) + 
  theme_bw()

###########
library(ISLR)
library(tidyverse)
?Credit
library(janitor)
credit <- Credit %>% 
  clean_names()

# Section 6.2.3
ggplot(data = credit, aes(x = income, y = balance)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()

mod2v <- lm(balance ~ limit + income, data = credit)
summary(mod2v)

###
BREAKS <- cut(credit$limit, 
              breaks = c(min(credit$limit), quantile(credit$limit, 
                                     probs = c(0.25, 0.50, 0.75)), 
                         max(credit$limit)), include.lowest = TRUE)
BREAKS
credit <- credit %>% 
  mutate(credit_limit = BREAKS)
head(credit)
ggplot(data = credit, aes(x = limit)) + 
  geom_histogram(fill = "lightblue") +
  geom_vline(xintercept = quantile(credit$limit, probs = c(0.25, 0.50, 0.75))) + 
  theme_bw()

p1 <- ggplot(data = credit, aes(x = income, y = balance)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()
p1

p2 <- ggplot(data = credit, aes(x = income, y = balance, color = credit_limit)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()
p2


summary(mod2v)
