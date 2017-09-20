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

ggplot(data = ND, aes(group, worms)) + 
  geom_point(position = "jitter", aes(color = group)) + 
  theme_bw()

ggplot(data = ND, aes(group, worms)) + 
  geom_boxplot() +
  theme_bw()


ans2 <- ND %>%
  group_by(group) %>%
  summarize(Mean = mean(worms), SD = sd(worms), 
            Median = median(worms), iqr = IQR(worms))
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

library(car)
qqPlot(Worms)
ggplot(data = ND, aes(sample = worms, color = group)) + 
  geom_qq()

# Tidy Verse
# Need to install oilabs
# devtools::install_github('OpenIntroOrg/oilabs-r-package')
library(oilabs)
rep_sample_n(ND, size = 10, reps = 2)
ND
sims <- 10^4 - 1
MDIFF <- rep_sample_n(ND, size = 10, reps = sims) %>%
          summarize(MD = mean(worms[6:10]) - mean(worms[1:5]))
pvalue <- (sum(MDIFF$MD >= 7.6) + 1)/(sims + 1)
pvalue
#
t.test(worms ~ group, data = ND)
#
ggplot(data = MDIFF, aes(x = MD)) + 
  geom_density(fill = "pink") + 
  theme_bw()
#


### Using replicate() now
f <- function(x){
  index <- sample(10, 5)
  MD <- mean(x[index]) - mean(x[-index])
  MD
}

ANS <- replicate(n = 10^4 - 1, expr = f(Worms))
pvalue <- (sum(ANS >= 7.6) + 1)/(10^4 - 1 + 1 )
pvalue





# prep work
x.dens <- density(MDIFF$MD)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
#
ggplot(data = MDIFF, aes(x = MD)) + 
  geom_density(fill = "pink") + 
  theme_bw() + 
  geom_area(data = subset(df.dens, x >= 7.6 & x <= max(MDIFF$MD)), 
            aes(x = x, y = y), fill = "red") + 
  labs(x = expression(bar(x)[Control] - bar(x)[Treatment]), 
       title = "Randomization Distribution")
# 
ggplot(data = MDIFF, aes(x = MD)) + 
  geom_histogram(fill = "pink", binwidth = 1, color = "black") + 
  geom_histogram(data = subset(MDIFF, MD >= 7.6), binwidth = 1, fill = "red", color = "black") + 
  theme_bw() +
  labs(x = expression(bar(x)[Control] - bar(x)[Treatment]), 
       title = "Randomization Distribution")

## Theoretical Now

# Given a vector of length n + m
# Take a resample of size m without replacement.
n <- 5
m <- 5
ncb <- choose(n + m, m)
ncb
CB <- t(combn(n + m, m))
head(CB)
nn <- dim(CB)[1]
nn
#
ND$worms

diffmeans <- numeric(nn)
for(i in 1:nn){
  diffmeans[i] <- mean(ND$worms[CB[i, ]]) - mean(ND$worms[-CB[i,]])
}
sort(diffmeans)
sum(diffmeans >= 7.6)
theo_pvalue <- mean(diffmeans >= 7.6)
theo_pvalue
MASS::fractions(theo_pvalue)
#
# Write a function!
rdtf <- function(x, y){
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  nx <- length(x)
  ny <- length(y)
  cv <- c(x, y)
  nn <- choose(nx + ny, nx)
  CB <- t(combn(nx + ny, nx))
  DM <- numeric(nn)
  for(i in 1:nn){
    DM[i] <- mean(cv[CB[i, ]]) - mean(cv[-CB[i, ]])
  }
  sort(DM)
}
#
rdtf(c(1, 2, 2, 10, 7), c(16, 10, 10, 7, 17))
#
rdtf(c(25, 33, 35, 38, 48, 55, 56), c(55, 55, 64))

# ChickWeight
ChickWeight %>% 
  group_by(Diet) %>%
  summarize(mean(weight), sd(weight))
#
ggplot(data = ChickWeight, aes(x = factor(Diet), y = weight)) +
  geom_boxplot() + 
  theme_bw()
###
Weight <- ChickWeight %>% 
  filter(Diet == "3" | Diet == "4") %>%
  select(weight, Diet)
Weight
Weight$weight