library(tidyverse)
gender <- c(rep("Female", 10), rep("Male", 10)) 
group <- rep(rep(c("Treatment", "Control"), each = 5), 2)
worms <- c(1, 2, 2, 10, 7, 16, 10, 10, 7, 17, 3, 5, 9, 10, 6, 31, 26, 28, 13, 47)
schis <- data.frame(gender, group, worms)
head(schis, n = 3)
ND <- schis %>% 
  filter(gender == "Female")
ND

library(tidyverse)
ND %>%
  group_by(group) %>%
  summarize(M = mean(worms), S = sd(worms), n = n())



diff(tapply(ND$worms, ND$group, mean))

tobs <- (12-4.4)/sqrt(4.3^2/5+3.91^2/5)
tobs

t.test(worms ~ group, data = ND)

-diff(tapply(ND$worms, ND$group, mean))
# xbar_Control - xbar_Treatment
obs_diff <- -diff(tapply(ND$worms, ND$group, mean))
obs_diff

P <- 10^4
per_mean_diff <- numeric(P)
for(i in 1:P){
  per_mean_diff[i] <- diff(tapply(ND$worms, sample(ND$group), mean))
}
hist(per_mean_diff)

pvalue <- sum(per_mean_diff >= obs_diff)/P
pvalue

# Or better yet

pvalue <- (sum(per_mean_diff >= obs_diff) + 1) / (P + 1)
pvalue

library(infer)
ND %>%
  specify(worms ~ group) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 10^3, type = "permute") %>%
  calculate(stat = "t") ->  tenleft
tenleft
visualize(tenleft)
sum(tenleft$stat >= 2.92)/10^3
get_p_value(tenleft, obs_stat = 2.92, direction = "greater")

library(PASWR2)
t.test(size ~ location, data = APTSIZE, alternative = "greater")
