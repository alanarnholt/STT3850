library(tidyverse)
gender <- c(rep("Female", 10), rep("Male", 10)) 
group <- rep(rep(c("Treatment", "Control"), each = 5), 2)
worms <- c(1, 2, 2, 10, 7, 16, 10, 10, 7, 17, 3, 5, 9, 10, 6, 31, 26, 28, 13, 47)
schis <- data.frame(gender, group, worms)
head(schis, n = 3)
ND <- schis %>% 
  filter(gender == "Female")
ND

tapply(ND$worms, ND$group, mean)
diff(tapply(ND$worms, ND$group, mean))
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

