# Week 4
# 
library(moderndive)
library(tidyverse)
library(PASWR2)
library(janitor)
names(VIT2005)
ggplot(data = VIT2005, aes(x = area, y = totalprice)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) + 
  xlim(0, 200) +
  ylim(0, 550000) 
  
# model
model_lm <- lm(totalprice ~ area, data = VIT2005)
get_regression_table(model_lm)

get_regression_table(model_lm) -> T1
T1

T1$estimate
b0 <- T1$estimate[1]
b1 <- T1$estimate[2]
c(b0, b1)

broom::tidy(model_lm) -> T2
T2$estimate
b0 <- T2$estimate[1]
b1 <- T2$estimate[2]
c(b0, b1)

summary(model_lm)$coef -> T3
T3
b0 <- T3[1, 1]
b1 <- T3[2, 1]
c(b0, b1)


ggplot(data = VIT2005, aes(x = area, y = totalprice)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) + 
  xlim(0, 200) +
  ylim(0, 550000) +
  geom_abline(slope = b1, intercept = b0, color = "red", lty = "dashed")

get_regression_points(model_lm) |> 
  arrange(desc(residual))

ggplot(data = VIT2005, aes(x = area, y = totalprice)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) 
