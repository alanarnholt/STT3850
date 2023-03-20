library(moderndive)
library(tidyverse)
set.seed(321)
population <- rnorm(5000, 100, 15)
MU <- mean(population)
SIGMA <- sd(population)
c(MU, SIGMA)
hist(population)
#
sample1_n25 <- sample(population, size = 25, replace = TRUE)
mean(sample1_n25)
sd(sample1_n25)

# 10000 samples of size n = 25
N <- 10000
xbar <- numeric(N)
for(i in 1:N){
  sampi <- sample(population, size = 25, replace = TRUE)
  xbar[i] <- mean(sampi)
}
hist(xbar)
mean(xbar)
sd(xbar)


# 10000 samples of size n = 49
N <- 10000
xbar <- numeric(N)
for(i in 1:N){
  sampi <- sample(population, size = 49, replace = TRUE)
  xbar[i] <- mean(sampi)
}
hist(xbar)
mean(xbar)
sd(xbar)


# 10000 samples of size n = 100
N <- 10000
xbar <- numeric(N)
for(i in 1:N){
  sampi <- sample(population, size = 100, replace = TRUE)
  xbar[i] <- mean(sampi)
}
hist(xbar)
mean(xbar)
sd(xbar)

######
DF <- data.frame(IQ = population)
str(DF)
head(DF)

# take a sample of size 50 from the population

DF %>% 
  rep_sample_n(size = 50, replace = TRUE) %>% 
  summarize(xbar = mean(IQ), SE = sd(IQ))

DF %>% 
  rep_sample_n(size = 25, replace = TRUE, reps = 10000) %>% 
  group_by(replicate) %>% 
  summarize(xbar = mean(IQ), SE = sd(IQ)) -> xbar25
p1 <- ggplot(data = xbar25, aes(x = xbar)) + geom_histogram() + 
  xlim(90, 110)
  
DF %>% 
  rep_sample_n(size = 50, replace = TRUE, reps = 10000) %>% 
  group_by(replicate) %>% 
  summarize(xbar = mean(IQ), SE = sd(IQ)) -> xbar50
p2 <- ggplot(data = xbar50, aes(x = xbar)) + geom_histogram() + 
  xlim(90, 110)

DF %>% 
  rep_sample_n(size = 100, replace = TRUE, reps = 10000) %>% 
  group_by(replicate) %>% 
  summarize(xbar = mean(IQ), SE = sd(IQ)) -> xbar100
p3 <- ggplot(data = xbar100, aes(x = xbar)) + geom_histogram() + 
  xlim(90, 110)

library(patchwork)
p1/p2/p3
#################
ggplot(tactile_prop_red, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red", 
       title = "Distribution of 33 proportions red") 
#
p <- mean(bowl$color == "red")
p
#
virtual_shovel <- bowl %>% 
  rep_sample_n(size = 50)
virtual_shovel
#
virtual_shovel %>% 
  mutate(is_red = (color == "red"))
#
virtual_shovel %>% 
  mutate(is_red = (color == "red")) %>% 
  summarize(num_red = sum(is_red))
#
virtual_shovel %>% 
  mutate(is_red = color == "red") %>% 
  summarize(num_red = sum(is_red)) %>% 
  mutate(prop_red = num_red / 50)
# My preference
virtual_shovel %>%
  summarize(phat = mean(color == "red"))
#
# Using the virtual shovel 33 times
virtual_samples <- bowl %>% 
  rep_sample_n(size = 50, reps = 33)
virtual_samples
#
virtual_samples %>% 
  group_by(replicate) %>% 
  summarize(phat = mean(color == "red")) -> virtual_prop_red
virtual_prop_red
#
ggplot(virtual_prop_red, aes(x = phat)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red", 
       title = "Distribution of 33 proportions red") 
#
# Using the virtual shovel 10000 times
virtual_samples <- bowl %>% 
  rep_sample_n(size = 50, reps = 10000)
virtual_samples
dim(virtual_samples)
virtual_prop_red <- virtual_samples %>% 
  group_by(replicate) %>% 
  summarize(phat = mean(color == "red"))
#
ggplot(virtual_prop_red, aes(x = phat)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red", 
       title = "Distribution of 10,000 proportions red") 
#####
virtual_prop_red %>% 
  summarize(mean_est_phat = mean(phat),
            sigma_est_phat= sd(phat))
#####
# Theoretical \mu_{\hat{p}} = p = 0.375
# \sigma_{\hat{p}} = \sqrt{p*(1-p)/n} = sqrt(0.375*(1-0.375)/50) = 0.06846532

# Use n = 25, 50, 100
set.seed(23)
virtual_samples_25 <- bowl %>% 
  rep_sample_n(size = 25, reps = 10000)
virtual_samples_25 %>% 
  group_by(replicate) %>% 
  summarize(phat = mean(color == "red")) -> v_sample_25_phat
v_sample_25_phat %>% 
  summarize(mu_est_phat = mean(phat),
            sigma_est_phat = sd(phat))
#
virtual_samples_50 <- bowl %>% 
  rep_sample_n(size = 50, reps = 10000)
virtual_samples_50 %>% 
  group_by(replicate) %>% 
  summarize(phat = mean(color == "red")) -> v_sample_50_phat
v_sample_50_phat %>% 
  summarize(mu_est_phat = mean(phat),
            sigma_est_phat = sd(phat))
#
virtual_samples_100 <- bowl %>% 
  rep_sample_n(size = 100, reps = 10000)
virtual_samples_100 %>% 
  group_by(replicate) %>% 
  summarize(phat = mean(color == "red")) -> v_sample_100_phat
v_sample_100_phat %>% 
  summarize(mu_est_phat = mean(phat),
            sigma_est_phat = sd(phat))
####
p1 <- ggplot(data = v_sample_25_phat, aes(x = phat)) + 
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") + 
  xlim(.1, .7)
p2 <- ggplot(data = v_sample_50_phat, aes(x = phat)) + 
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") + 
  xlim(.1, .7)
p3 <- ggplot(data = v_sample_100_phat, aes(x = phat)) + 
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") + 
  xlim(.1, .7)
library(patchwork)
p1 / p2 / p3
