library(tidyverse)
library(PASWR2)
VIT2005
ggplot(data = VIT2005, aes(x = area, y = totalprice)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE)
#
cor(VIT2005$totalprice, VIT2005$area)
VIT2005 %>%
  summarize(r = cor(totalprice, area))

mod_lsr <- lm(totalprice ~ area, data = VIT2005)
mod_lsr

library(moderndive)
get_regression_table(mod_lsr)
get_regression_points(mod_lsr) -> rp
rp$residual

summary(mod_lsr)
mod_lsr
head(predict(mod_lsr))
head(rp$totalprice_hat)
mod_lsr$fitted
mod_lsr$residuals
mod_lsr$coef
