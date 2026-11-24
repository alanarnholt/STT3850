# Practive inclass
# P(T_6 >= 2) = 0.04621316
curve(dt(x, 6), -4, 4)
pt(2, 6, lower.tail = FALSE)
# P(-1.25 <= Z <= 2.5) = 0.8881406
curve(dnorm(x), -3.4, 3.4)
pnorm(2.5, 0, 1) - pnorm(-1.25, 0, 1)
# P(X^2_10 <= 12.5) = 0.7470147
curve(dchisq(x, 10), 0, 25)
pchisq(12.5, 10)
# P(a <= Z <= 1.5) = 0.70 -> a = -0.7283725
curve(dnorm(x), -3.4, 3.4)
qnorm(pnorm(1.5) - .70)
# P(0.5 <= X^2_6 <= 8.7) = 0.8066725
curve(dchisq(x, 6), 0, 20)
pchisq(8.7, 6) - pchisq(0.5, 6)

########
library(tidyverse)
library(moderndive)
library(infer)
gss
xtabs(~sex + college, data = gss) -> T1
T1
addmargins(T1)
(c_11_exp <- 263*326/500)
(c_21_exp <- 237*326/500)
(c_12_exp <- 263*174/500)
(c_22_exp <- 237*174/500)
chisq.test(T1, correct = FALSE)
chisq.test(T1, correct = FALSE)$exp -> EXP
EXP
chi_obs <- sum((T1 - EXP)^2/EXP)
chi_obs
pvalue <- pchisq(chi_obs, 1, lower.tail = FALSE)
pvalue

xtabs(~sex + partyid, data = gss)
xtabs(~sex + partyid, data = gss)[,1:3] -> T2
T2
chisq.test(T2, correct = FALSE)

library(PASWR2)
TITANIC3 <- TITANIC3 |> 
  mutate(survived = factor(survived, labels = c("Dead", "Alive")))

xtabs(~pclass + survived, data = TITANIC3) -> T3
T3
ggplot(data = TITANIC3, aes(x = survived, fill = pclass)) + 
  geom_bar(position = "fill")
chisq.test(T3, correct = FALSE)
chis_obs <- chisq.test(T3, correct = FALSE)$stat
chis_obs
B <- 10^4
chi_stat <- numeric(B)
for(i in 1:B){
  chi_stat[i] <- chisq.test(xtabs(~pclass + sample(survived), data = TITANIC3), correct = FALSE)$stat
}
hist(chi_stat)






xtabs(~survived + sex, data = TITANIC3) -> T4
T4
chisq.test(T4, correct = FALSE)

## DO the test with infer now

TITANIC3 |> 
  specify(pclass ~ survived) |> 
  hypothesize(null = "independence") |> 
  generate(reps = 10^4, type = "permute") |> 
  calculate(stat = "Chisq") -> PD
PD
visualize(PD) 
visualize(PD) + 
  shade_p_value(obs_stat = 365.89, direction = "greater")
