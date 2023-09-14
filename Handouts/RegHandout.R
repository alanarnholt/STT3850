
knitr::opts_chunk$set(echo = TRUE, comment = NA, fig.align = "center", warning = FALSE, message = FALSE)


#| label: "fig-fourlines"
#| fig-cap: "Four possible results for a single dummy variable with two levels.  Graph I has the intercept and the slope the same for both levels of the dummy variable.  Graph II has the two lines with the same slope, but different intercepts.  Graph III shows the two fitted lines with the same intercept but different slopes.  Graph IV shows the two lines with different intercepts and different slopes."
opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow=c(2, 2))
par(mar = c(4, 4, 4, 2))
par(las = 1)
x <- seq(0, 10, .1)
y <- x
plot(x, y, type = "n",  axes = FALSE, xlab = "", ylab = "", main = "I")
arrows(0, 0, 0, 10, code = 2, length = 0.1)
arrows(0, 0, 10, 0, code = 2, length = 0.1)
segments(1, 3, 9, 7)
mtext(text = "x", side = 1, line = 0, at = 5)
mtext(text = "Y", side = 2, line = 0, at = 5)
plot(x, y, type = "n",  axes = FALSE, xlab = "", ylab = "", main = "II")
arrows(0, 0, 0, 10, code = 2, length = 0.1)
arrows(0, 0, 10, 0, code = 2, length = 0.1)
segments(1, 3, 9, 5)
segments(1, 5, 9, 7)
mtext(text = "x", side = 1, line = 0, at = 5)
mtext(text = "Y", side = 2, line = 0, at = 5)
plot(x, y, type = "n",  axes = FALSE, xlab = "", ylab = "", main = "III")
arrows(0, 0, 0, 10, code = 2, length = 0.1)
arrows(0, 0, 10, 0, code = 2, length = 0.1)
segments(0, 3, 9, 5)
segments(0, 3, 9, 8)
mtext(text = "x", side = 1, line = 0, at = 5)
mtext(text = "Y", side = 2, line = 0, at = 5)
plot(x, y, type = "n",  axes = FALSE, xlab = "", ylab = "", main = "IV")
arrows(0, 0, 0, 10, code = 2, length = 0.1)
arrows(0, 0, 10, 0, code = 2, length = 0.1)
segments(1, 3, 9, 8)
segments(1, 4, 9, 5)
mtext(text = "x", side = 1, line = 0, at = 5)
mtext(text = "Y", side = 2, line = 0, at = 5)
par(opar)


#| label: "fig-sol1"
#| fig-cap: "Scatterplot of `totalprice` versus `area` with the fitted regression line superimposed from `mod_simple`"
library(tidyverse)
library(PASWR2)
VIT2005 <- VIT2005 %>% 
  mutate(elevator = factor(elevator, labels = c("No", "Yes")))
mod_simple <- lm(totalprice ~ area, data = VIT2005)
summary(mod_simple)
library(moderndive)
get_regression_table(mod_simple)
ggplot(data = VIT2005, aes(x = area, y = totalprice)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE)



mod_full <- lm(totalprice ~ area + elevator + area:elevator, data = VIT2005)
anova(mod_simple, mod_full)  # compare models



anova(mod_full)



mod_full <- lm(totalprice ~ area + elevator + area:elevator, data = VIT2005)
mod_inter <- lm(totalprice ~ area + area:elevator, data = VIT2005)
anova(mod_inter, mod_full)  # compare models



summary(mod_inter)
coef(mod_inter)
b0 <- coef(mod_inter)[1]
b1NO <- coef(mod_inter)[2]
b1YES <- coef(mod_inter)[2] + coef(mod_inter)[3]
c(b0, b1NO, b1YES)


#| label: "fig-DSSI"
#| fig-cap: "Fitted regression lines for `mod_inter`"
ggplot(data = VIT2005, aes(x = area, y = totalprice, color = elevator)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_abline(intercept = b0, slope = b1NO, color = "red") + 
  geom_abline(intercept = b0, slope = b1YES, color = "blue") + 
  scale_color_manual(values = c("red", "blue")) + 
  xlim(10, 200) + 
  ylim(50000, 500000) + 
  labs(x = "Living Area is Square Meters",
       y = "Appraised Price in Euros")



MDF <- get_regression_points(mod_inter)
ggplot(data = MDF, aes(x = totalprice_hat, y = residual)) +
  geom_point() +
  theme_bw() +
  labs(title = "Residuals versus Fitted Values") +
  geom_hline(yintercept = 0, lty = "dashed")



ggplot(data = MDF, aes(x = residual)) +
  geom_histogram(fill = "lightblue", color = "blue") +
  theme_bw() 

ggplot(data = MDF, aes(sample = residual)) +
  geom_qq() +
  geom_qq_line() +
  theme_bw() 



ggplot(data = MA_schools, 
       aes(x = perc_disadvan, y = average_sat_math)) + 
  geom_point() +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE)
mod_simple <- lm(average_sat_math ~ perc_disadvan, 
                 data = MA_schools)
summary(mod_simple)
get_regression_table(mod_simple)



ggplot(data = MA_schools, 
       aes(x = perc_disadvan, y = average_sat_math, color = size)) + 
  geom_point() +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) -> p1
ggplot(data = MA_schools, 
       aes(x = perc_disadvan, y = average_sat_math, color = size)) + 
  geom_point() +
  theme_bw() + 
  geom_parallel_slopes(se = FALSE) -> p2
library(patchwork)
p1 + p2
mod_full <- lm(lm(average_sat_math ~ perc_disadvan + size + perc_disadvan:size, data = MA_schools))
anova(mod_simple, mod_full)
anova(mod_full)



library(ISLR)
credit_paradox <- Credit %>% 
  select(ID, debt = Balance, credit_limit = Limit, 
         credit_rating = Rating, income = Income, age = Age)
ggplot(data = credit_paradox, aes(x = credit_limit, y = debt)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() -> p1
ggplot(data = credit_paradox, aes(x = income, y = debt)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() -> p2
library(patchwork)
p1 + p2



library(plotly)
p <- plot_ly(data = credit_paradox, z = ~debt, x = ~credit_limit, y = ~income) %>% 
  add_markers()
p
mod <- lm(debt ~ credit_limit + income, data = credit_paradox)
summary(mod)$coef
x <- seq(min(credit_paradox$credit_limit), max(credit_paradox$credit_limit), length = 70)
y <- seq(min(credit_paradox$income), max(credit_paradox$income), length = 70) 
plane <- outer(x, y, function(a, b){coef(mod)[1] + coef(mod)[2]*a + coef(mod)[3]*b})
# draw the plane
p %>% 
  add_surface(x = ~x, y = ~y, z = ~plane)



qs <- quantile(credit_paradox$credit_limit, probs = seq(0, 1, .25))
# credit_paradox$credit_cats <- cut(credit_paradox$credit_limit, breaks = qs, include.lowest = TRUE)
############### Or above
credit_paradox <- credit_paradox %>% 
  mutate(credit_cats = cut(credit_limit, breaks = qs, include.lowest = TRUE))
head(credit_paradox)


ggplot(data = credit_paradox, aes(x = credit_limit)) +
  geom_density(fill = "pink", color = "black") + 
  geom_vline(xintercept = qs, color = "blue") + 
  theme_bw()

credit_paradox %>% 
  group_by(credit_cats) %>% 
  summarize(n())



p1 <- ggplot(data = credit_paradox, aes(x = income, y = debt)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() + 
  labs(y = "Credit card debt (in $)",
       x = "Income (in $1000)")
p2 <- ggplot(data = credit_paradox, aes(x = income, y = debt, color = credit_cats)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() + 
  labs(y = "Credit card debt (in $)",
       x = "Income (in $1000)",
       color = "Credit limit bracket")
p1 + p2

