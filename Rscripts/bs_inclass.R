library(resampledata)
Bangladesh
B <- 10000
bang <- Bangladesh
cobalt <- bang$Cobalt[-252]
summary(cobalt)
bs_sd <- numeric(B)
for(i in 1:B){
  bss <- sample(cobalt, size = 270, replace = TRUE)
  bs_sd[i] <- sd(bss)
}
hist(bs_sd)

quantile(bs_sd, probs = c(.05, 0.95))

library(infer)
library(tidyverse)
bang |> 
  specify(response = Cobalt) |> 
  generate(reps = B, type = "bootstrap") |> 
  calculate(stat = "sd") -> bs_dist
visualize(bs_dist)
get_confidence_interval(bs_dist, level = .90)

library(tidyverse)
library(moderndive)
library(infer)

?TV
head(TV)
TV |> 
  group_by(Cable) |> 
  summarize(M = mean(Times), n = n()) -> ST
ST
diff_actual <- ST$M[1] - ST$M[2]
diff_actual
TV |> 
  filter(Cable == "Basic") |> 
  select(Times) |> 
  pull() -> basic_times
basic_times
TV |> 
  filter(Cable == "Extended") |> 
  select(Times) |> 
  pull() -> extended_times
extended_times
B <- 10^5
diff_means <- numeric(B)
for(i in 1:B){
  basic_bss <- sample(basic_times, size = 10, replace = TRUE)
  extended_bss <- sample(extended_times, size = 10, replace = TRUE)
  diff_means[i] <- mean(basic_bss) - mean(extended_bss)
}
hist(diff_means)
# 90% Bootstap Percentile CI
quantile(diff_means, probs = c(0.05, 0.95)) -> BPCI
BPCI

######### Using the infer pipeline
TV |> 
  specify(Times ~ Cable) |> 
  generate(reps = 10^4, type = "bootstrap") |> 
  calculate(stat = "diff in means", order = c("Basic", "Extended")) -> bs_dist
visualize(bs_dist) + 
  theme_bw()
get_confidence_interval(bs_dist, level = .90, type = "percentile")
get_confidence_interval(bs_dist, type = "se", point_estimate = diff_actual, level = 0.90)
diff_actual + c(-1,1)*qnorm(.95)*sd(bs_dist$stat)


Sat2008 |> 
  specify(response = Math) |> 
  generate(reps = 10^5, type = "bootstrap") |> 
  calculate(stat = "mean") -> bs_math_dist
bs_math_dist
get_confidence_interval(bs_math_dist, level = 0.90)

math <- Sat2008$Math
math
B <- 10^5
bs_mean <- numeric(B)
for(i in 1:B){
  bss <- sample(math, size = sum(!is.na(math)), replace = TRUE)
  bs_mean[i] <- mean(bss)
}
hist(bs_mean)
quantile(bs_mean, probs = c(0.05, 0.95))

boot_bias <- mean(bs_mean) - mean(math)
boot_bias

library(PASWR2)
# BODYFAT |> 
#   specify(fat ~ sex) |> 
#   generate(reps = 10^4, type = "bootstrap") |> 
#   calculate(stat = "diff in means", order = c("F", "M")) -> bs_dist
# get_confidence_interval(bs_dist, level = 0.95)


BODYFAT$fat[BODYFAT$sex=="M"] -> mf
BODYFAT$fat[BODYFAT$sex=="F"] -> ff

B <- 10^4
dm <- numeric(B)
for(i in 1:B){
  bssf <- sample(ff, size = 14, replace = TRUE)
  bssm <- sample(mf, size = 4, replace = TRUE)
  dm[i] <- mean(bssf) - mean(bssm)
} 
hist(dm)
quantile(dm, probs = c(.025, 0.975))


##############################################################