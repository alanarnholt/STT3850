##################
## Alan T. Arnholt
## 11/06/2023
##################
library(tidyverse)
library(moderndive)
pennies ### This is the "population"
(N <- dim(pennies)[1])
(MU <- mean(pennies$year))
(SIGMA <- sd(pennies$year)*(N-1)/N) # sigma = sqrt((x_i - mu)^2/N)
## Suppose we take a single sample of size n = 49 from the population
## We want to test H_0: mu = 1987 versus H_1: mu > 1987 
set.seed(64)
sample49 <- rep_sample_n(pennies, size = 49, replace = FALSE, reps = 1)
sample49
(xbar <- mean(sample49$year))
ggplot(data = sample49, aes(x = year)) + 
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
## which  is 1990.245---so, to make the null true we need to subtract
## 1990.245 - 1987 = 3.245 from all values in "year"
set.seed(23)
sample49 %>% 
  mutate(year = year - 3.245) -> recenter
(mean(recenter$year))
recenter %>% 
  rep_sample_n(size = 49, replace = TRUE, reps = 10000) %>% 
  group_by(replicate) %>% 
  summarize(stat = mean(year)) -> null_distribution
ggplot(data = null_distribution, aes(x = stat)) + 
  geom_histogram(binwidth = 5/7, fill = "lightblue", color = "black") + 
  theme_bw() + 
  geom_vline(xintercept = xbar, color = "purple", linetype = "dashed") + 
  geom_vline(xintercept = 1987, color = "red") + 
  labs(title = "Null Distribution")
(pv <- mean(null_distribution$stat >= xbar))

#########
#########
## Doing the same thing with the infer pipeline
##
library(infer)
set.seed(32)
sample49 %>% 
  specify(response = year) %>% 
  hypothesize(null = "point", mu = 1987) %>% 
  generate(reps = 10000, type = "bootstrap") %>% # Note that for one sample - bootstrap
  calculate(stat = "mean") -> dist_null
visualize(dist_null) + 
  shade_pvalue(obs_stat = xbar, direction = "right", 
               color = "purple", fill = "blue") + 
  theme_bw()
get_pvalue(dist_null, obs_stat = xbar, direction = "right")


### Note that once we specified the null hypothesis, the infer pipeline
### took care of re-centering the null distribution.

### Same thing with a for loop now
set.seed(321)
Year <- sample49$year
head(Year)
Year <- Year - 3.245 # Recenter so the null is true!
B <- 10^4
bstat <- numeric(B)
for(i in 1:B){
  bss <- sample(Year, size = 49, replace = TRUE)
  bstat[i] <- mean(bss)
}
hist(bstat, col = "lightblue", 
     breaks = "Scott", main = "Null Distribution")
abline(v = xbar, col = "blue", lt = "dashed")
(pv <- mean(bstat >= xbar))
######
# Note
SIGMA/sqrt(49) # SE(xbar)
sd(bstat)      # as an approximation
t.test(sample49$year, mu = 1987, alternative = "greater")

