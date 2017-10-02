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
