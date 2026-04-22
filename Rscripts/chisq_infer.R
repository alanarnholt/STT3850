library(infer)
library(tidyverse)
library(moderndive)
# Create a contingency table using gss for the variables sex and partyid
xtabs(~sex + partyid, data = gss) -> T1
T1
gss |> 
  select(sex, partyid) |> 
  table() -> T2
T2
table(gss$sex, gss$partyid) -> T3
T3
with(data = gss, table(sex, partyid)) -> T4
T4
chisq.test(T1)$expected
# This gives us problems!  Expected counts < 5
T5 <- T1[,1:3]
T5
chisq.test(T5, correct = FALSE)
chisq.test(T5, correct = FALSE)$expected
# The expected counts for other are < 5 so the test may not return valid results.
# Let us use permutation testing.
gss |>  
  filter(partyid != "DK" & partyid != "other") |> 
  mutate(partyid = droplevels(partyid)) -> newDF 
newDF |> 
  select(sex, partyid) |> 
  table() -> T7
T7
#### Bar plot
newDF |> 
  ggplot(aes(x = partyid, fill = sex)) + 
  geom_bar(position = "fill")
chisq.test(T7, correct = FALSE)
chisq.test(T7, correct = FALSE)$stat -> obsT 
obsT
set.seed(47)
P <- 10^4
stat <- numeric(P)
for(i in 1:P){
  stat[i] <- chisq.test(xtabs(~sex + sample(partyid), data = newDF), correct = FALSE)$stat
}
hist(stat)
(pvalue_loop <- mean(stat >= obsT))

newDF |> 
  specify(sex ~ partyid) |> 
  hypothesize(null = "independence") |> 
  calculate(stat = "Chisq") -> obs_stat1
obs_stat1

newDF |> 
  specify(sex ~ partyid) |> 
  hypothesize(null = "independence") |> 
  generate(reps = 10^4, type = "permute") |> 
  calculate(stat = "Chisq") -> null_distA
null_distA
visualize(null_distA) +
  shade_p_value(obs_stat = obs_stat1, direction = "greater")
get_p_value(null_distA, obs_stat = obs_stat1, direction = "greater")
###
mean(null_distA$stat >= obs_stat1$stat)

# Infer is going to automatically drop the DK column
gss |> 
  specify(sex ~ partyid) |> 
  hypothesize(null = "independence") |> 
  calculate(stat = "Chisq") -> obs_stat
obs_stat
gss |> 
  specify(sex ~ partyid) |> 
  hypothesize(null = "independence") |> 
  generate(reps = 10^4, type = "permute") |> 
  calculate(stat = "Chisq") -> null_dist
null_dist
visualize(null_dist) +
shade_p_value(obs_stat = obs_stat, direction = "greater")
get_p_value(null_dist, obs_stat = obs_stat, direction = "greater")
###
mean(null_dist$stat >= obs_stat$stat)
############ Test to see if sex and college are independent ##############
gss |> 
  select(sex, college) |> 
  table() -> TA
TA
chisq.test(TA, correct = FALSE)
chisq.test(TA, correct = FALSE)$expected -> exp
chisq.test(TA, correct = FALSE)$observed -> obs 
exp
obs
chi_obs <- sum((obs - exp)^2/exp)
chi_obs
pvalue1 <- pchisq(chi_obs, 1, lower = FALSE)
pvalue1
# OR
pvalue2 <- chisq.test(TA, correct = FALSE)$p.value
pvalue2
chi_obs2 <- chisq.test(TA, correct = FALSE)$stat
chi_obs2

