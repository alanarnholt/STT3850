library(dplyr)
# anscombe
apply(anscombe, 2, mean)
apply(anscombe, 2, sd)
hist(anscombe$x1)  
hist(anscombe$x4)
plot(y1 ~ x1, data = anscombe)
plot(y2 ~ x2, data = anscombe)
plot(y3 ~ x3, data = anscombe)
plot(y4 ~ x4, data = anscombe)
# 
# Direction, Form, Strength, Outliers
library(ggplot2)
ggplot(data = anscombe, aes(x = x1, y = y1)) + geom_point() + theme_bw() + geom_smooth(method = "lm")
ggplot(data = anscombe, aes(x = x2, y = y2)) + geom_point() + theme_bw() + geom_smooth(method = "lm")
ggplot(data = anscombe, aes(x = x3, y = y3)) + geom_point() + theme_bw() + geom_smooth(method = "lm")
ggplot(data = anscombe, aes(x = x4, y = y4)) + geom_point() + theme_bw() + geom_smooth(method = "lm")
anscombe %>% 
  summarize(cor(x1, y1))
# 
# VIT2005


library(mosaicData)
head(SAT)
library(dplyr)
library(ggplot2)
ggplot(data = SAT, aes(x = salary, y = sat)) + 
  geom_point() + 
  theme_bw()
mod <- lm(sat ~ salary, data = SAT)
summary(mod)
###
SAT %>% 
  summarize(COR = cor(sat, salary))
mod2 <- lm(sat ~ salary + frac, data = SAT)
summary(mod2)
#
SAT <- SAT %>% 
  mutate(sat_bin = cut(frac, 3))
SAT
## 
ggplot(data = SAT, aes(x = salary, y = sat, color = sat_bin)) + 
  geom_point() + 
  theme_bw()
##
ggplot(data = SAT, aes(x = salary, y = sat, color = sat_bin)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE)
##
mod3 <- lm(sat ~ salary + sat_bin, data = SAT)
summary(mod3)
## How to graph mod3?
ggplot(data = SAT, aes(x = salary, y = sat, color = sat_bin)) + 
  geom_point() + 
  theme_bw() +
  geom_line(data = broom::augment(mod3), aes(y  = .fitted)) 
## With labels now!
ggplot(data = SAT, aes(x = salary, y = sat, color = sat_bin)) + 
  geom_point() + 
  geom_text(aes(label = state), vjust = -0.5, hjust = 0) + 
  theme_bw() +
  geom_line(data = broom::augment(mod3), aes(y  = .fitted)) 

