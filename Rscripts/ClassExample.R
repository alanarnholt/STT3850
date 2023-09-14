library(tidyverse)
library(PASWR2)
?VIT2005
ggplot(data = VIT2005, aes(x = area, y = totalprice)) +
  geom_point(color = "orange", size = 5, alpha = .2) + 
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE)
cor(VIT2005$totalprice, VIT2005$area)
VIT2005 %>% 
  summarize(r = cor(totalprice, area))
VIT2005 %>% 
  mutate(z_x = (area - mean(area))/sd(area), 
         z_y = (totalprice - mean(totalprice))/sd(totalprice)) %>% 
  select(area, totalprice, z_x, z_y) -> JU
JU
JU %>% 
  summarize(r = sum(z_x*z_y)/217)


# 


modVIT <- lm(totalprice ~ area, data = VIT2005)

library(moderndive)
get_regression_table(modVIT)

pt(-3.35, 216)*2


summary(VIT2005)
summary(modVIT)
summary(modVIT)$coef
names(summary(modVIT))
# Conf Interval
confint(modVIT, level = .90)


get_regression_points(modVIT) -> T1
head(T1$residual)
head(modVIT$resid)
head(T1$totalprice_hat)
head(modVIT$fitted)
head(predict(modVIT))
modVIT$coef[1] + modVIT$coef[2]*120

predict(modVIT, newdata = data.frame(area = c(60, 80, 100, 120, 140)))
