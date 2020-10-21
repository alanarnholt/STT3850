# Bootstrapping Mercury
# 10/20/2020
# Problem 5.13 MSWR

library(infer)
library(tidyverse)
library(resampledata)

head(FishMercury)
dim(FishMercury)
hist(FishMercury$Mercury)
#
ggplot(data = FishMercury, aes(x = Mercury)) +
  geom_histogram() +
  theme_bw()
#
# Bootstrap the mean, and record the bootstrap standard error and the 95% bootstrap percentile interval
#
b <- 10^4
stat <- numeric(b)
for(i in 1:b){
  bs <- sample(FishMercury$Mercury, 30, replace = TRUE)
  stat[i] <- mean(bs)
}
(bsse <- sd(stat))
hist(stat)
(CI <- quantile(stat, probs = c(0.025, 0.975)))
### Same with infer
IM <- FishMercury %>% 
  specify(response = Mercury) %>% 
  generate(reps = 10^4, type = "bootstrap") %>% 
  calculate(stat = "mean")

visualize(IM)
get_confidence_interval(IM, level = 0.95)
(CI <- quantile(stat, probs = c(0.025, 0.975)))

# Remove the outlier and bootstrap the mean of the remaining data.
FishMercuryC <- FishMercury %>% 
  filter(Mercury <= 1.8) 
summary(FishMercuryC)

b <- 10^4
stat <- numeric(b)
for(i in 1:b){
  bs <- sample(FishMercuryC$Mercury, 30, replace = TRUE)
  stat[i] <- mean(bs)
}
(bsse <- sd(stat))
hist(stat)
(CI <- quantile(stat, probs = c(0.025, 0.975)))
### Same with infer
IM <- FishMercuryC %>% 
  specify(response = Mercury) %>% 
  generate(reps = 10^4, type = "bootstrap") %>% 
  calculate(stat = "mean")

visualize(IM)
get_confidence_interval(IM, level = 0.95)



###################################################
###################################################
###################################################
## STuff for DataCamp

library(tidyverse)
library(infer)
library(openintro)
manhattan <- read_csv("https://assets.datacamp.com/production/repositories/846/datasets/bd62fb71666052ffe398d85e628eae9d0339c9c4/manhattan.csv")
glimpse(manhattan)
hist(manhattan$rent)
ggplot(data = manhattan, aes(x = rent)) +
  geom_histogram(binwidth = 500) + 
  theme_bw()

B <- 15000
stat <- numeric(B)
for(i in 1:B){
  bs <- sample(manhattan$rent, size = sum(!is.na(manhattan$rent)), replace = TRUE)
  stat[i] <- median(bs)
}
hist(stat)
PCI <- quantile(stat, probs = c(0.025, 0.975))
PCI
rent_med_obs <- median(manhattan$rent)
rent_med_obs
###
CI <- rent_med_obs + c(-1, 1)*qt(.975, 19)*sd(stat)
CI
###
str(ncbirths) # from openintro package


####


B <- 15000
stat <- numeric(B)
for(i in 1:B){
  bs <- sample(nc_complete$visits, size = sum(!is.na(nc_complete$visits)), replace = TRUE)
  stat[i] <- mean(bs)
}
hist(stat)
####
PCI <- quantile(stat, probs = c(0.025, 0.975))
PCI

CI <- mean(nc_complete$visits) + c(-1, 1)*qt(0.975, sum(!is.na(nc_complete$visits)) -1)*sd(stat)
CI

B <- 15000
stat <- numeric(B)
for(i in 1:B){
  bs <- sample(nc_complete$visits, size = sum(!is.na(nc_complete$visits)), replace = TRUE)
  stat[i] <- sd(bs)
}
hist(stat)
####
PCI <- quantile(stat, probs = c(0.025, 0.975))
PCI

#### Recentering Test for median price
med_rent_obs <- median(manhattan$rent)
med_rent_obs
B <- 15000
stat <- numeric(B)
for(i in 1:B){
  bs <- sample(manhattan$rent, size = sum(!is.na(manhattan$rent)), replace = TRUE)
  stat[i] <- median(bs) + 150
}
hist(stat)
pvalue <- mean(stat >= med_rent_obs)
pvalue

####
obs_mean <- mean(ncbirths$weight)
obs_mean

B <- 15000
stat <- numeric(B)
for(i in 1:B){
  bs <- sample(ncbirths$weight, size = sum(!is.na(ncbirths$weight)), replace = TRUE)
  stat[i] <- mean(bs) - 0.101
}
hist(stat)
pvalue <- mean(stat >= obs_mean)*2
pvalue

