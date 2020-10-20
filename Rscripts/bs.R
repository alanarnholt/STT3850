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
