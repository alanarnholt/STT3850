library(moderndive)
library(tidyverse)
glimpse(evals)

ggplot(data = evals, aes(x = age, y = score, color = gender)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()


ggplot(data = evals, aes(x = age, y = score, color = gender)) + 
  geom_point() + 
  geom_parallel_slopes(se = FALSE) + 
  theme_bw()


library(ISLR)
credit <- Credit
ggplot(data = credit, aes(x = Income, y = Balance, color = Ethnicity)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)


ggplot(data = credit, aes(x = Income, y = Balance, color = Ethnicity)) + 
  geom_point() + 
  geom_parallel_slopes(se = FALSE)
# ModPar
mod_par <- lm(Balance ~ Income + Ethnicity, data = credit)
mod_int <- lm(Balance ~ Income + Ethnicity + Income:Ethnicity, data = credit)

anova(mod_par, mod_int)
anova(mod_int)

summary(mod_par)


# CI for E(Y_h) when Income = 100 and Ethnicity = Asian
predict(mod_par, newdata = data.frame(Income = 100, 
                                      Ethnicity = "Asian"),
        interval = "conf", level = 0.90, se.fit = TRUE)
# S_Yh = 51.8802

(CI <- c(850.0185 + c(-1, 1) *qt(.95, 396)*51.8802))
y <- credit$Balance
X <- model.matrix(mod_par)
(xh <- matrix(c(1, 100, 1, 0), nrow = 1))

S2 <- MSE*xh%*%solve(t(X)%*%X)%*%t(xh)
(S <- sqrt(S2))
beta_hat <- solve(t(X)%*%X)%*%t(X)%*%y
beta_hat
(MSE <- summary(mod_par)$sigma^2)
(XTXI <- summary(mod_par)$cov)
(vc <- MSE*XTXI)

