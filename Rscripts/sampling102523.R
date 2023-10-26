age <- c(37, 98, 9, 1, 42, 5, 57)

hist(age)

B <- 10000
bsm <- numeric(B)
for(i in 1:B){
  bss <- sample(age, size = sum(!is.na(age)), replace = TRUE)
  bsm[i] <- mean(bss)
}
c(mean(bsm), sd(bsm))
hist(bsm)
quantile(bsm, probs = c(.025, 0.975))

# Using infer
library(infer)
library(tidyverse)

DF <- data.frame(age = age)
DF %>% 
  specify(response = age) %>% 
  generate(reps = 10000, type = "bootstrap") %>% 
  calculate(stat = "mean") -> BS
BS
visualize(BS)
visualize(BS, bins = 25, dens_color = "red")

##################################################
# SE CI
(CISE <- get_ci(BS, point_estimate = mean(age), 
                type = "se", level = .90))
mean(age) +c(-1, 1)*qnorm(.95)*sd(BS$stat)
##################################################
# Percentile CI
get_ci(BS, level = .90)
BPCI <- quantile(BS$stat, probs = c(.05, .95))
BPCI
##################################################
visualize(BS, bins = 20) +
shade_confidence_interval(endpoints = BPCI) + 
  theme_bw()
##################################################
ggplot(data = BS, aes(x = stat)) + 
  geom_histogram(fill = "lightblue", color = "blue") + 
  theme_bw() + 
  geom_vline(xintercept = BPCI, color = "red")
###################################################
library(PASWR2)
names(VIT2005)

(obs_stat <- mean(VIT2005$totalprice)/mean(VIT2005$area))
B <- 10000
mystat <- numeric(B)
for(i in 1:B){
  bssP <- sample(VIT2005$totalprice, size = sum(!is.na(VIT2005$totalprice)), replace = TRUE)
  bssA <- sample(VIT2005$area, size = sum(!is.na(VIT2005$area)), replace = TRUE)
  mystat[i] <- mean(bssP)/mean(bssA)
}
hist(mystat)
