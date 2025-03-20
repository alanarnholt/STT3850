### Chapter 7 Code
n <- 50
p <- 0.34
set.seed(32)
balls <- rbinom(n = 1, size = n, prob = p)
balls
(phat <- balls/n) 
###
# Repeat the process 33 times
B <- 33
phat <- numeric(B)
for(i in 1:B){
  balls <- rbinom(n = 1, size = 50, prob = 0.34)
  phat[i] <- balls/50
}
hist(phat)
# Or
library(tidyverse)
DF <- tibble(phat = phat)
ggplot(data = DF, aes(x = phat)) + 
  geom_histogram(binwidth = 0.025, 
                 color = "black", 
                 fill = "pink",
                 boundary = 0.40) + 
  theme_bw()
#
library(moderndive)
tactile_prop_red
ggplot(data = tactile_prop_red, aes(x = prop_red)) + 
  geom_histogram(binwidth = 0.05, 
                 color = "black", 
                 fill = "pink") + 
  theme_bw()

ggplot(data = tactile_prop_red, aes(x = prop_red)) + 
  geom_histogram(binwidth = 0.05,
                 boundary = 0.4, 
                 color = "black", 
                 fill = "pink") + 
  theme_bw()

#### 7.2.1
bowl
####
set.seed(123)
virtual_shovel <- bowl |> 
  rep_sample_n(size = 50)
virtual_shovel
virtual_shovel |> 
  summarize(num_red = sum(color == "red"),
            prop_red = sum(color =="red")/50)
### Use the virtual shovel 33 times
virtual_samples <- bowl |> 
  rep_sample_n(size = 50, reps = 33)
virtual_samples

####### Compute the prop_red for each of the 33 samples/replicates
virtual_samples |> 
  group_by(replicate) |> 
  summarize(num_red = sum(color == "red"),
            prop_red = sum(color =="red")/50) -> virtual_prop_red
virtual_prop_red
quantile(virtual_prop_red$prop_red)
quantile(virtual_prop_red$prop_red, probs = c(0.01, 0.5, 0.99))

virtual_prop_red |> 
  summarize(Q10 = quantile(prop_red, 0.1),
            Q90 = quantile(prop_red, 0.9))
#### Create a histogram of prop_red from 33 samples

ggplot(data = virtual_prop_red, aes(x = prop_red)) + 
  geom_histogram(color = "black",
                 fill = "pink",
                 boundary = 0.4, 
                 binwidth = 0.05) + 
  theme_bw()
                 
### Too much variability lets generate 10,000 samples
### versus 33 samples to study the sampling distribution
set.seed(123)
virtual_samples <- bowl %>% 
  rep_sample_n(size = 50, reps = 10000)
virtual_samples

virtual_samples |> 
  group_by(replicate) |> 
  summarize(num_red = sum(color == "red"),
            prop_red = sum(color =="red")/50) -> virtual_prop_red
virtual_prop_red

ggplot(data = virtual_prop_red, aes(x = prop_red)) + 
  geom_histogram(color = "black",
                 fill = "pink",
                 boundary = 0.4, 
                 binwidth = 0.05) + 
  theme_bw()

### What is the actual proportion of 
### red_balls greater than 0.4 in virtual_prop_red?  # 0.41
virtual_prop_red |> 
  summarize(pg = mean(prop_red >= 0.4))
### Q? - what is the actual proportion of red_balls
### between 0.3 and 0.5 inclusive? # 0.869
virtual_prop_red |> 
  summarize(ans = mean(prop_red >= 0.3 & prop_red <= 0.5))
###
### approximate the distribution of prop_red using the 
### normal distribution.
### phat ~ N(mu_phat = 0.375, sigma_phat = sqrt(0.375 * (1 - .375)/50))
### phat ~ N(0.375, 0.06846532)

pnorm(0.4, 0.375, 0.06846532, lower = FALSE)  # 0.3575003
#
pnorm(0.5, 0.375, 0.06846532) - pnorm(0.3, 0.375, 0.06846532) # 0.8293946
### Note the normal approximation is not that good. Actual: 0.869 vs approx: 0.8293946

### Increase n to 100.
set.seed(321)
virtual_samples <- bowl %>% 
  rep_sample_n(size = 100, reps = 10000)
virtual_samples

virtual_samples |> 
  group_by(replicate) |> 
  summarize(num_red = sum(color == "red"),
            prop_red = sum(color =="red")/100) -> virtual_prop_red
virtual_prop_red

### graph dist of prop_red
ggplot(data = virtual_prop_red, aes(x = prop_red)) + 
  geom_histogram(color = "black",
                 fill = "pink",
                 boundary = 0.4, 
                 binwidth = 0.01) + 
  theme_bw()

## Compute actual 0.35 <= prop_red <= 0.40  - ANS: 0.475

virtual_prop_red |> 
  summarize(ansactual = mean(prop_red >= 0.35 & prop_red <= 0.40))

## Approximate the answer using the normal distribution
## phat ~ N(0.375, sqrt(0.375*(1 - .375)/100))
## phat ~ N(0.375, 0.04841229)

pnorm(0.4, 0.375, sqrt(0.375*(1 - .375)/100)) - pnorm(0.35, 0.375, sqrt(0.375*(1 - .375)/100)) # 0.3944234

## Note that the approximation is not very good - actual 0.475 versus
## approximation of 0.3944234.

## If we want to use the normal approximation we need to sample from
## a population....with more than 2400 members.
## Note we can not take a larger sample from the population with N = 2400
## without running into independence issues.  We would need to apply a
## finite population correction factor if we leave the pop at N = 2400 
## and take a larger sample size.  Consider a pop with 1000 members.

set.seed(44125)
n <- 1000
p <- 0.375
balls <- rbinom(n = 1, size = n, prob = p)
balls
(phat <- balls/n) 
###
# Repeat the process 10000 times
B <- 10000
phat <- numeric(B)
for(i in 1:B){
  balls <- rbinom(n = 1, size = 1000, prob = 0.375)
  phat[i] <- balls/1000
}
hist(phat)
# Or
library(tidyverse)
DF <- tibble(phat = phat)
ggplot(data = DF, aes(x = phat)) + 
  geom_histogram(binwidth = 0.002, 
                 color = "black", 
                 fill = "pink",
                 boundary = 0.4) + 
  theme_bw()

### 0.35 and 0.4 actual proportion.
mean(phat)
sd(phat)
mean(phat >= 0.35 & phat <= 0.4) # 0.9095
#
pnorm(0.4, 0.375, sqrt(0.375*(1 - .375)/1000)) - pnorm(0.35, 0.375, sqrt(0.375*(1 - .375)/1000)) # 0.8975296

mean(phat <= 0.35) # 0.0494
pnorm(0.35, 0.375, sqrt(0.375*(1 - .375)/1000)) # 0.05123522
 