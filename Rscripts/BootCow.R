# Alan Arnholt
# Class: Bootstrapping: percentile CIs and bootstrap T CIs

# First Examine butterfat versus age
library(PASWR2)
library(tidyverse)

ggplot(data = COWS, aes(x = butterfat)) +
  geom_histogram(fill = "lightblue", color = "black", binwidth = 0.25) +
  facet_grid(rows = vars(age)) +
  theme_bw()

COWS %>% 
  group_by(age) %>% 
  summarize(M = mean(butterfat), S = sd(butterfat), n = n())

#####

ggplot(data = COWS, aes(sample=butterfat)) +
  stat_qq(color = "blue") + 
  facet_grid(rows = vars(age)) + 
  stat_qq_line(color = "red")

# Classical 95% CI for mu_2yo - mu_mature

t.test(butterfat ~ age, data = COWS, conf = 0.95)

library(infer)
COWS %>%
  specify(butterfat ~ age) %>%
  generate(reps = 10^4, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("2 years old", "Mature")) -> BSD
get_confidence_interval(BSD, level = .90)
visualize(BSD)
# 95% Bootstrap percentile CI for mu_2yo - mu_mature
set.seed(13)
COWS %>% filter(age == "2 years old") %>% select(butterfat) %>% pull() -> bf2yo
COWS %>% filter(age == "Mature") %>% select(butterfat) %>% pull() -> bfmat
B <- 10^4
md <- numeric(B)
for(i in 1:B){
  bsstyo <- sample(bf2yo, size = sum(!is.na(bf2yo)), replace = TRUE)
  bssmat <- sample(bfmat, size = sum(!is.na(bfmat)), replace = TRUE)
  md[i] <- mean(bsstyo) - mean(bssmat)
}

CI <- quantile(md, probs = c(0.025, 0.975))
CI

# 98% Bootstrap T CI for mu_2yo - mu_mature

set.seed(13)
COWS %>% filter(age == "2 years old") %>% select(butterfat) %>% pull() -> bf2yo
COWS %>% filter(age == "Mature") %>% select(butterfat) %>% pull() -> bfmat
n_2yo <- sum(!is.na(bf2yo))
n_mat <- sum(!is.na(bfmat))
c(n_2yo, n_mat)
B <- 10^4
Q <- numeric(B)
for(i in 1:B){
  bsstyo <- sample(bf2yo, size = sum(!is.na(bf2yo)), replace = TRUE)
  bssmat <- sample(bfmat, size = sum(!is.na(bfmat)), replace = TRUE)
  Q[i] <- ( (mean(bsstyo) - mean(bssmat)) - (mean(bf2yo) - mean(bfmat)) ) / sqrt(var(bsstyo)/n_2yo + var(bssmat)/n_mat) 
}

QS <- quantile(Q, probs = c(0.05, 0.95))
QS
CIBT <- c((mean(bf2yo) - mean(bfmat)) - QS[2]*sqrt(var(bf2yo)/n_2yo + var(bfmat)/n_mat),
          (mean(bf2yo) - mean(bfmat)) - QS[1]*sqrt(var(bf2yo)/n_2yo + var(bfmat)/n_mat)  )
CIBT


#### Another Approach

B <- 10^4 
TS <- numeric(B)
for(i in 1:B){
  TS[i] <- t.test(butterfat ~ sample(age), data = COWS)$stat
}
QS <- quantile(TS, probs = c(0.05, 0.95))
CIBT <- c((mean(bf2yo) - mean(bfmat)) - QS[2]*sqrt(var(bf2yo)/n_2yo + var(bfmat)/n_mat),
          (mean(bf2yo) - mean(bfmat)) - QS[1]*sqrt(var(bf2yo)/n_2yo + var(bfmat)/n_mat)  )
CIBT 

##### A third approach

COWS %>% 
  specify(butterfat ~ age) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 10^4, type = "permute") %>% 
  calculate(stat = "t") -> TDIST
quantile(TDIST$stat, probs = c(0.05, 0.95)) -> QS
QS
CIBT <- c((mean(bf2yo) - mean(bfmat)) - QS[2]*sqrt(var(bf2yo)/n_2yo + var(bfmat)/n_mat),
          (mean(bf2yo) - mean(bfmat)) - QS[1]*sqrt(var(bf2yo)/n_2yo + var(bfmat)/n_mat)  )
CIBT   


# Construct a 95% bootstrap percentile CI for mu_2yo/mu_mat

B <- 10^4
rm <- numeric(B)
for(i in 1:B){
  bss1 <- sample(bf2yo, 50, replace = TRUE)
  bss2 <- sample(bfmat, 50, replace = TRUE)
  rm[i] <- mean(bss1)/mean(bss2)
}
quantile(rm, probs = c(0.025, 0.975))







B <- 10^4
dm <- numeric(B)
for(i in 1:B){
  bss1 <- sample(bf2yo, 50, replace = TRUE)
  bss2 <- sample(bfmat, 50, replace = TRUE)
  dm[i] <- mean(bss1, trim = 0.2) - mean(bss2, trim = 0.2)
}
quantile(dm, probs = c(0.025, 0.975))










##############################################
library(PASWR2)
library(tidyverse)

gender <- c(rep("Female", 10), rep("Male", 10)) 
group <- rep(rep(c("Treatment", "Control"), each = 5), 2)
worms <- c(1, 2, 2, 10, 7, 16, 10, 10, 7, 17, 3, 5, 9, 10, 6, 31, 26, 28, 13, 47)
schis <- data.frame(gender, group, worms)
head(schis, n = 3)
ND <- schis %>% 
  filter(gender == "Female")
ND
ND %>%
  group_by(group) %>%
  summarize(Mean = mean(worms), SD = sd(worms), 
            Median = median(worms), iqr = IQR(worms)) -> ans2
ans2

###### Theoretical Permutation Distribution

n <- 5
m <- 5
ncb <- choose(m + n, m)
ncb
#########
# CONSIDER:
combn(3, 2)
choose(3, 2)
combn(4, 2)
choose(4, 2)

CB <- t(combn(n + m, m))
head(CB)

##########

nn <- dim(CB)[1]
nn

###########

ND$worms


###########

diffmeans <- numeric(nn)
for(i in 1:nn){
  diffmeans[i] <- mean(ND$worms[CB[i, ]]) - mean(ND$worms[-CB[i,]])
}
sort(diffmeans)

###########
# Write a Function

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


rdtf(c(16, 10, 10, 7, 17), c(1, 2, 2, 10, 7))
mean(c(27, 19, 12, 19, 21, 26))
mean(c(26, 20, 19, 22, 19, 19))
rdtf(c(27, 19, 12, 19, 21, 26), c(26, 20, 19, 22, 19, 19))
##############
library(infer)
data(gss)
gss %>% 
  group_by(college) %>% 
  summarize(Mean = mean(age), n = n()) -> ans3
ans3
obs_diff <- diff(ans3$Mean) # degree - no degree
obs_diff
tapply(gss$age, gss$college, mean)
diff(tapply(gss$age, gss$college, mean))
obs_diff1 <- diff(tapply(gss$age, gss$college, mean))
obs_diff1

B <- 10^4
gss %>% 
  specify(age~college) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = B, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("degree", "no degree")) -> pd
pd

get_p_value(pd, obs_stat = obs_diff1, direction = "two-sided")

visualize(pd) +
  shade_p_value(obs_stat = obs_diff1, direction = "two-sided", color = "blue", fill = "lightblue")

###################
## Testing with a for loop

dm <- numeric(B)
for(i in 1:B){
  dm[i] <- diff(tapply(gss$age, sample(gss$college), mean))
}
hist(dm)
abline(v = c(-obs_diff, obs_diff), col = "blue")

#############################################################################################
#############################################################################################

library(tidyverse)
library(NHANES)
colnames(NHANES)
sleep <- NHANES %>% 
  select(SleepTrouble, SleepHrsNight) %>%
  filter(!is.na(SleepTrouble), !is.na(SleepHrsNight))
dim(sleep)

sleep <- na.omit(sleep)

ggplot(data = sleep, aes(x = SleepHrsNight)) +
  geom_density(bw = .4) + 
  facet_grid(rows = vars(SleepTrouble))

sleep %>% 
  group_by(SleepTrouble) %>% 
  summarize(Median = median(SleepHrsNight), 
            Mean = mean(SleepHrsNight), 
            Skew = e1071::skewness(SleepHrsNight),
            SD = sd(SleepHrsNight),
            iqr = IQR(SleepHrsNight), 
            n = n())


sleep %>% 
  group_by(SleepTrouble) %>% 
  summarize(Mean = mean(SleepHrsNight, na.rm = TRUE), n = n(), SKEW = e1071::skewness(SleepHrsNight))

############
# Construct a 90% percentile bootstrap CI for the average sleep time for Americans with 
# and without sleep trouble.

library(infer) 

sleep %>% 
  filter(SleepTrouble == "No") %>% 
  specify(response = SleepHrsNight) %>% 
  generate(10^4, type = "bootstrap") %>% 
  calculate(stat = "mean") -> xbar
get_confidence_interval(xbar, level = 0.90) -> BPCI
BPCI

### Same thing with a for loop
sleep %>% 
  filter(SleepTrouble == "No") %>% 
  select(SleepHrsNight) %>% 
  pull() -> shnts
B <- 10^4
ybar <- numeric(B)
for(i in 1:B){
  bss <- sample(shnts, size = length(shnts), replace = TRUE)
  ybar[i] <- mean(bss)
}
quantile(ybar, probs = c(0.05, 0.95)) -> BPCI2
BPCI2

### Bootstrap t 90% CI
B <- 10^4
TS <- numeric(B)
for(i in 1:B){
  bss <- sample(shnts, size = length(shnts), replace = TRUE)
  TS[i] <- (mean(bss) - mean(shnts))/(sd(bss)/sqrt(length(bss)))
}
quantile(TS, probs = c(0.05, 0.95)) -> QS
QS
BTCI <- c(mean(shnts) - QS[2]*sd(shnts)/sqrt(length(shnts)),
          mean(shnts) - QS[1]*sd(shnts)/sqrt(length(shnts)))
BTCI

###############################################################
### Test H0: mu = 7.15 hours vs HA: mu < 7.15 hours for Americans with no sleep trouble.
sleep %>% 
  filter(SleepTrouble == "No") %>% 
  specify(response = SleepHrsNight) %>% 
  hypothesize(null = "point", mu = 7.15) %>% 
  generate(10^4, type = "bootstrap") %>% 
  calculate(stat = "mean") -> test
hist(test$stat)
(obs_mean <- mean(shnts))
get_p_value(test, obs_stat = obs_mean, direction = "less")




delta <- 7.15 - obs_mean
B <- 10^4
testo <- numeric(B)
for(i in 1:B){
  bss <- sample(shnts, size = length(shnts), replace = TRUE) + delta
  testo[i] <- mean(bss)
}
hist(testo)
pvalue <- (sum(testo <= obs_mean) + 1)/(B + 1)
pvalue

