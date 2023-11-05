##################
library(tidyverse)
library(moderndive)
pennies ### This is the "population"
(MU <- mean(pennies$year))
## Suppose we take a single sample of size n = 49 from the population
## We want to test H_0: mu = 1987 versus H_1: mu > 1987 
set.seed(64)
sample50 <- rep_sample_n(pennies, size = 50, replace = FALSE, reps = 1)
sample50
(xbar <- mean(sample50$year))
ggplot(data = sample50, aes(x = year)) + 
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") + 
  theme_bw() + 
  geom_vline(xintercept = xbar, color = "purple", linetype = "dashed") + 
  geom_vline(xintercept = 1987, color = "red")
########################################################################
##
## Using tidyverse
##
## Generate the null distribution
## Under the assumption H_0 is true--- mu = 1987
## Currently the values are centered at the purple dashed line
## which  is 1990.32---so, to make the null true we need to subtract
## 1990.32 - 1987 = 3.32 from all values in "year"

sample50 %>% 
  mutate(year = year - 3.32) -> recenter
(mean(recenter$year))
recenter %>% 
  rep_sample_n(size = 49, replace = TRUE, reps = 1000) %>% 
  group_by(replicate) %>% 
  summarize(stat = mean(year)) -> null_distribution
ggplot(data = null_distribution, aes(x = stat)) + 
  geom_histogram(binwidth = 5/7, fill = "lightblue", color = "black") + 
  theme_bw() + 
  geom_vline(xintercept = xbar, color = "purple", linetype = "dashed") + 
  geom_vline(xintercept = 1987, color = "red") + 
  labs(title = "Null Distribution")
(pv <- mean(null_distribution >= xbar))

#########
#########
## Doing the same thing with the infer pipeline
##
set.seed(32)
sample50 %>% 
  specify(response = year) %>% 
  hypothesize(null = "point", mu = 1987) %>% 
  generate(reps = 1000, type = "bootstrap") %>% # Note that for one sample - bootstrap
  calculate(stat = "mean") -> dist_null
visualize(dist_null)
get_pvalue(dist_null, obs_stat = xbar, direction = "right")


### Note that once we specified the null hypothesis, the infer pipeline
### took care of re-centering the null distribution.

### Same thing with a for loop now

Year <- sample50$year
head(Year)
Year <- Year - 3.32 # Recenter so the null is true!
B <- 10^3
bstat <- numeric(B)
for(i in 1:B){
  bss <- sample(Year, size = 49, replace = TRUE)
  bstat[i] <- mean(bss)
}
hist(bstat)
(pv <- mean(bstat >= xbar))
######
t.test(sample50$year, mu = 1987, alternative = "greater")

