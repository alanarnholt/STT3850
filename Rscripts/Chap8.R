# Chapter 8
library(moderndive)
library(tidyverse)
############################
pennies_sample
B <- 10000
bsm <- numeric(B)
for(i in 1:B){
  bss <- sample(pennies_sample$year, size = dim(pennies_sample)[1], replace = TRUE)
  bsm[i] <- mean(bss)
}
hist(bsm)
CI <- quantile(bsm, probs = c(0.025, 0.975))
CI
mean(bsm) + c(-1, 1)*qnorm(.975)*sd(bsm)
############################
pennies_sample %>% 
  rep_sample_n(size = 50, replace = TRUE, reps = B) %>% 
  group_by(replicate) %>% 
  summarize(bsm = mean(year)) -> stuff
ggplot(data = stuff, aes(x = bsm)) + 
  geom_histogram()
BCI <- stuff %>% 
  summarize(lep = quantile(bsm, .025), uep = quantile(bsm, 0.975))
BCI
###
library(infer)
pennies_sample %>% 
  specify(response = year) %>% 
  generate(reps = B, type = "bootstrap") %>% 
  calculate(stat = "mean") -> bsd 
head(bsd)
ggplot(data = bsd, aes(x = stat)) +
  geom_histogram()
# or 
visualize(bsd)
get_confidence_interval(bsd, level = 0.95)
get_confidence_interval(bsd, type = "se", point_estimate = mean(bsd$stat))
#
bsd %>% 
  summarize(lep = quantile(stat, 0.025), uep = quantile(stat, 0.975)) -> BCI
BCI
#####
library(tidyverse)
library(resampledata)
dim(NCBirths2004)
names(NCBirths2004)
head(NCBirths2004)
NCBirths2004 %>% group_by(Gender) %>% 
  summarize(M = mean(Weight), S = sd(Weight), n = n()) -> ans
ans
ans$M[1] + c(-1, 1)*qt(.995, 520)*ans$S[1]/sqrt(ans$n[1])


NCBirths2004 %>% filter(Gender == "Female") %>%
  select(Weight) -> ans1
ans1 %>% pull() -> femaleweight
femaleweight
t.test(femaleweight, conf.level = .99)



NCBirths2004 %>% filter(Gender == "Female") %>%
  select(Weight) %>% t.test(conf.level = .99)
t.test(Weight ~ Gender, data = NCBirths2004, conf.level = .90)
qt(.95, 1002.9)


########################
sims <- 10^5
ts <- numeric(sims)
for(i in 1:sims){
  rs <- rnorm(9, 17, 3)
  ts[i] <- (mean(rs) - 17)/(sd(rs)/3)
}
quantile(ts, probs = c(.05, 0.95))


sims <- 10^5
ts <- numeric(sims)
for(i in 1:sims){
  rs <- rexp(9, .1)
  ts[i] <- (mean(rs) - 10)/(sd(rs)/3)
}
quantile(ts, probs = c(.05, 0.95))

rx <- rexp(10000, .1)
hist(rx)
mean(rx)
sd(rx)







# Bootstrap T
# (xbar* - xbar)/(s*/sqrt(n))

library(tidyverse)
library(resampledata)
dim(NCBirths2004)
NCBirths2004 %>% filter(Gender == "Female") %>%
  select(Weight) -> ans1
ans1 %>% pull() -> femaleweight
femaleweight

# Bootstrap Percentile CI
B <- 10^4
bmean <- numeric(B)
for(i in 1:B){
  bss <- sample(femaleweight, size = length(femaleweight), replace = TRUE)
  bmean[i] <- mean(bss)
}
CIBP <- quantile(bmean, probs = c( 0.025, 0.975))
CIBP

# Bootstrap T CI
B <- 10^4
BT <- numeric(B)
for(i in 1:B){
  bss <- sample(femaleweight, size = length(femaleweight), replace = TRUE)
  BT[i] <- (mean(bss) - mean(femaleweight))/(sd(bss)/sqrt(length(femaleweight)))
}
CT <- quantile(BT, probs = c(0.025, 0.975))
CT

CIT <- c(mean(femaleweight) - CT[2]*sd(femaleweight)/sqrt(length(femaleweight)), 
        mean(femaleweight) - CT[1]*sd(femaleweight)/sqrt(length(femaleweight)) )
CIT

btci1m <- function(data, B = 10^4, conf.level = 0.95){
  alpha <- 1 - conf.level
  bt <- numeric(B)
  n <- length(data)
  for(i in 1:B){
    bss <- sample(data, size = n, replace = TRUE)
    bt[i] <- (mean(bss) - mean(data))/(sd(data)/sqrt(n))
  }
  ct <- quantile(bt, probs = c(alpha/2, 1 - alpha/2)) 
  CI <- c( mean(data) - ct[2]*sd(data)/sqrt(n), 
          mean(data) - ct[1]*sd(data)/sqrt(n) )
  names(CI) <- c("lower end point", "upper end point")
  hist(bt, main = "Bootstrap T*", breaks = "Scott")
  CI
}

library(tidyverse)
library(resampledata)
head(Bangladesh)
cholorine <- Bangladesh %>% select(Chlorine) %>% pull()
summary(cholorine)
cholorine <- na.omit(cholorine)
btci1m(cholorine, B = 10^3)

