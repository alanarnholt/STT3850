# Alan Arnholt
# 9/17/17

gender <- c(rep("Female", 10), rep("Male", 10)) 
group <- rep(rep(c("Treatment", "Control"), each = 5), 2)
worms <- c(1, 2, 2, 10, 7, 16, 10, 10, 7, 17, 3, 5, 9, 10, 6, 31, 26, 28, 13, 47)
schis <- data.frame(gender, group, worms)
head(schis, n = 3)

library(ggplot2)
p <- ggplot(data = schis, aes(group, worms)) + 
  geom_point(position = "jitter", aes(color = group)) + 
  facet_grid(. ~ gender) + 
  theme_bw()
p

library(dplyr)
ans <- schis %>%
  group_by(gender, group) %>%
  summarize(Median = median(worms), Mean = mean(worms), SD = sd(worms))
ans
# Write hypotheses:
# Write fundamental question of inference:

ND <- schis[gender=="Female", ]
ND

ans2 <- ND %>%
  group_by(group) %>%
  summarize(Mean = mean(worms), SD = sd(worms))
ans2

obs.diff <- ans2[1, 2] - ans2[2, 2]
obs.diff
obs.diff$Mean

Worms <- ND$worms

sims <- 10^4-1
sim.diff <- numeric(sims)
for(i in 1:sims){
  index <- sample(10, 5, replace = FALSE)
  sim.diff[i] <- mean(Worms[index]) - mean(Worms[-index])
}
pvalue <- (sum(sim.diff >= obs.diff$Mean) + 1)/(sims + 1)
pvalue

library(cars)
qqPlot(Worms)
