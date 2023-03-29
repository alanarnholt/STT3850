# Chapter 8
library(moderndive)
library(tidyverse)
############################
pennies_sample
B <- 10000
bsm <- numeric(B)
for(i in 1:B){
  bss <- sample(pennies_sample$year, size = dim(pennies_sample)[1], replace = TRUE)
  bsm[i] <- mean(bss)
}
hist(bsm)
CI <- quantile(bsm, probs = c(0.025, 0.975))
CI
mean(bsm) + c(-1, 1)*qnorm(.975)*sd(bsm)
############################
pennies_sample %>% 
  rep_sample_n(size = 50, replace = TRUE, reps = B) %>% 
  group_by(replicate) %>% 
  summarize(bsm = mean(year)) -> stuff
ggplot(data = stuff, aes(x = bsm)) + 
  geom_histogram()
BCI <- stuff %>% 
  summarize(lep = quantile(bsm, .025), uep = quantile(bsm, 0.975))
BCI
###
library(infer)
pennies_sample %>% 
  specify(response = year) %>% 
  generate(reps = B, type = "bootstrap") %>% 
  calculate(stat = "mean") -> bsd 
head(bsd)
ggplot(data = bsd, aes(x = stat)) +
  geom_histogram()
# or 
visualize(bsd)
get_confidence_interval(bsd, level = 0.95)
get_confidence_interval(bsd, type = "se", point_estimate = mean(bsd$stat))
#
bsd %>% 
  summarize(lep = quantile(stat, 0.025), uep = quantile(stat, 0.975)) -> BCI
BCI
#####