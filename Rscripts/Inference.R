## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, comment = NA, fig.align = "center")
library(tidyverse)

## ------------------------------------------------------------------------
OD <- matrix(data = c(28, 6, 19, 7), nrow = 2, byrow = TRUE)
dimnames(OD) <- list(Location = c("East", "West"), Drink = c("Cola", "Orange"))
ODT <- as.table(OD)
ODTDF <- as.data.frame(ODT)
DDF <- as.tbl(vcdExtra::expand.dft(ODTDF))
DDF
xtabs(~Location + Drink, data = DDF)
DDF %>%
  group_by(Location) %>%
  summarize(Pcola = mean(Drink == "Cola"), Porange = mean(Drink == "Orange"))
# shuffle the data
set.seed(13)
T1 <- xtabs(~Location + sample(Drink), data = DDF)
T1
prop.table(T1, 1)

## ------------------------------------------------------------------------
ggplot(data = DDF, aes(x = Location, fill = Drink)) +     
  geom_bar(position = "fill") + 
  labs(y = "Fraction") + 
  theme_bw() + 
  scale_fill_manual(values = c("brown", "orange"))

## ------------------------------------------------------------------------
set.seed(1)
DDF %>%
  mutate(Drink_perm = sample(Drink)) %>% 
  group_by(Location) %>%
  summarize(prop_cola_perm = mean(Drink_perm == "Cola"),
            prop_cola = mean(Drink == "Cola")) %>% 
  summarize(diff_perm = diff(prop_cola_perm),
            diff_orig = diff(prop_cola))

## ------------------------------------------------------------------------
set.seed(1)
T1 <- xtabs(~Location + Drink, data = DDF)
T1
T1P <- prop.table(T1, 1)
T1P
TR <- xtabs(~Location + sample(Drink), data = DDF)
TR
TRP <- prop.table(TR, 1)
TRP
diff_orig <- T1P[2, 1] - T1P[1, 1]
diff_orig
diff_perm <- TRP[2, 1] - TRP[1, 1]
diff_perm

## ------------------------------------------------------------------------
library(oilabs)
DDF %>%
  rep_sample_n(size = nrow(DDF), reps = 5) %>% 
  mutate(Drink_perm = sample(Drink)) %>% 
  group_by(replicate, Location) %>%
  summarize(prop_cola_perm = mean(Drink_perm == "Cola"),
            prop_cola = mean(Drink == "Cola")) %>% 
  summarize(diff_perm = diff(prop_cola_perm),
            diff_orig = diff(prop_cola))

## ------------------------------------------------------------------------
system.time(DDF %>%
  rep_sample_n(size = nrow(DDF), reps = 10000) %>% 
  mutate(Drink_perm = sample(Drink)) %>% 
  group_by(replicate, Location) %>%
  summarize(prop_cola_perm = mean(Drink_perm == "Cola"),
            prop_cola = mean(Drink == "Cola")) %>% 
  summarize(diff_perm = diff(prop_cola_perm),
            diff_orig = diff(prop_cola)) %>% 
ggplot(aes(x = diff_perm)) + 
  geom_histogram(binwidth = 0.07, fill = "pink", color = "black") + 
  theme_bw())
#
DDF %>%
  rep_sample_n(size = nrow(DDF), reps = 10000) %>% 
  mutate(Drink_perm = sample(Drink)) %>% 
  group_by(replicate, Location) %>%
  summarize(prop_cola_perm = mean(Drink_perm == "Cola"),
            prop_cola = mean(Drink == "Cola")) %>% 
  summarize(diff_perm = diff(prop_cola_perm),
            diff_orig = diff(prop_cola)) %>% 
ggplot(aes(x = diff_perm)) + 
  geom_histogram(binwidth = 0.07, fill = "pink", color = "black") + 
  theme_bw() + 
  labs(x = expression(hat(p)[W] - hat(p)[E]))

## ------------------------------------------------------------------------
set.seed(34)
DDF %>%
  rep_sample_n(size = nrow(DDF), reps = 10000) %>% 
  mutate(Drink_perm = sample(Drink)) %>% 
  group_by(replicate, Location) %>%
  summarize(prop_cola_perm = mean(Drink_perm == "Cola"),
            prop_cola = mean(Drink == "Cola")) %>% 
  summarize(diff_perm = diff(prop_cola_perm),
            diff_orig = diff(prop_cola)) %>% 
  ggplot(aes(x = diff_perm)) + 
  geom_density(fill = "purple", adjust = 2) + 
  theme_bw() + 
  labs(x = expression(hat(p)[W] - hat(p)[E]))

## ------------------------------------------------------------------------
T1 <- xtabs(~Location + Drink, data = DDF)
T1
PT <- prop.table(T1, 1)
PT
pd <- PT[2, 1] - PT[1, 1]
pd
# Repeat this process many times
sims <- 10^4 - 1
PD <- numeric(sims)
for(i in 1:sims){
  T1 <- xtabs(~Location + sample(Drink), data = DDF)
  PT <- prop.table(T1, 1)
  PD[i] <- PT[2, 1] - PT[1, 1]
}
ggplot(data = data.frame(x = PD), aes(x = x)) + 
  geom_histogram(binwidth = 0.07, fill = "pink", color = "black") + 
  theme_bw() + 
  labs(x = expression(hat(p)[W] - hat(p)[E]))

## ------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(NHANES)

## ------------------------------------------------------------------------
names(NHANES)

## ---- label = "NHB", fig.cap = "Segmented barplot of home ownership by gender"----
ggplot(NHANES, aes(x = Gender, fill = HomeOwn)) + 
  geom_bar(position = "fill") +
  ylab("Relative frequencies")

## ---- warning = TRUE, message = TRUE-------------------------------------
ggplot(NHANES, aes(x = SleepHrsNight, color = SleepTrouble)) + 
  geom_density(adjust = 2) + 
  facet_wrap(~ HealthGen) + 
  theme_bw()

## ------------------------------------------------------------------------
homes <- NHANES %>%
  select(Gender, HomeOwn) %>%
  filter(HomeOwn %in% c("Own", "Rent"))
dim(homes)

## ------------------------------------------------------------------------
homes %>%
  mutate(HomeOwn_perm = sample(HomeOwn)) %>%
  group_by(Gender) %>%
  summarize(prop_own_perm = mean(HomeOwn_perm == "Own"), 
            prop_own = mean(HomeOwn == "Own"))
# Perform one permutation 
homes %>%
  mutate(HomeOwn_perm = sample(HomeOwn)) %>%
  group_by(Gender) %>%
  summarize(prop_own_perm = mean(HomeOwn_perm == "Own"), 
            prop_own = mean(HomeOwn == "Own")) %>%
  summarize(diff_perm = diff(prop_own_perm),
            diff_orig = diff(prop_own))
# Another View
T1 <- xtabs(~Gender + HomeOwn, data = homes)
T1
prop.table(T1, 1)

## ------------------------------------------------------------------------
homes %>% 
  rep_sample_n(size = 5, reps = 3)

## ---- eval = FALSE-------------------------------------------------------
## # Perform 10 permutations - Starter Code
## homeown_perm <- homes %>%
##   rep_sample_n(size = ___, reps = ___) %>%
##   mutate(HomeOwn_perm = ___) %>%
##   group_by(___, ___) %>%
##   summarize(prop_own_perm = ___,
##             prop_own = ___) %>%
##   summarize(diff_perm = ___,
##             diff_orig = ___) # male - female

## ------------------------------------------------------------------------
# Perform 10 permutations
homeown_perm <- homes %>%
  rep_sample_n(size = nrow(homes), reps = 10) %>%
  mutate(HomeOwn_perm = sample(HomeOwn)) %>%
  group_by(replicate, Gender) %>%
  summarize(prop_own_perm = mean(HomeOwn_perm == "Own"), 
            prop_own = mean(HomeOwn == "Own")) %>%
  summarize(diff_perm = diff(prop_own_perm),
            diff_orig = diff(prop_own)) # male - female

## ------------------------------------------------------------------------
# Print differences to console
homeown_perm

## ------------------------------------------------------------------------
# Dotplot of 10 permuted differences in proportions
ggplot(homeown_perm, aes(x = diff_perm)) + 
  geom_dotplot(binwidth = 0.001, fill = "red") + 
  theme_bw() + 
  labs(x = expression(hat(p)[M]-hat(p)[F]))

## ------------------------------------------------------------------------
# Perform 500 permutations
homeown_perm <- homes %>%
  rep_sample_n(size = nrow(homes), reps = 500) %>%
  mutate(HomeOwn_perm = sample(HomeOwn)) %>%
  group_by(replicate, Gender) %>%
  summarize(prop_own_perm = mean(HomeOwn_perm == "Own"), 
            prop_own = mean(HomeOwn == "Own")) %>%
  summarize(diff_perm = diff(prop_own_perm),
            diff_orig = diff(prop_own)) # male - female
# Dotplot of 500 permuted differences in proportions
ggplot(homeown_perm, aes(x = diff_perm)) + 
  geom_dotplot(binwidth = 0.001, fill = "purple") +
  theme_bw() + 
  labs(x = expression(hat(p)[M] - hat(p)[F]))

## ------------------------------------------------------------------------
# Perform 10000 permutations
homeown_perm <- homes %>%
  rep_sample_n(size = nrow(homes), reps = 10000) %>%
  mutate(HomeOwn_perm = sample(HomeOwn)) %>%
  group_by(replicate, Gender) %>%
  summarize(prop_own_perm = mean(HomeOwn_perm == "Own"), 
            prop_own = mean(HomeOwn == "Own")) %>%
  summarize(diff_perm = diff(prop_own_perm),
            diff_orig = diff(prop_own)) # male - female

## ------------------------------------------------------------------------
P = (3254 + 3171)/9712
sed <- sqrt(P*(1 - P)*(1/4890 + 1/4822))
c(P, sed)
ggplot(homeown_perm, aes(x = diff_perm)) + 
  geom_density(fill = "pink") + 
  labs(x = expression(hat(p)[M] - hat(p)[F])) + 
  theme_bw() + 
  geom_vline(xintercept = c(-0.007828723, 0.007828723), linetype = "dashed") 

## ------------------------------------------------------------------------
homeown_perm %>% 
  summarize(pvalue = mean(diff_perm <= diff_orig) + mean(diff_perm >= -diff_orig))
# or
homeown_perm %>% 
  summarize(pvalue = min(mean(diff_perm <= diff_orig), mean(diff_perm >= -diff_orig))*2)

## ------------------------------------------------------------------------
GD <- matrix(data = c(21, 3, 14, 10), nrow = 2, byrow = TRUE)
dimnames(GD) <- list(sex = c("male", "female"), promote = c("promoted", "not_promoted"))
GDT <- as.table(GD)
GDTDF <- as.data.frame(GDT)
disc <- as.tbl(vcdExtra::expand.dft(GDTDF))
xtabs(~sex + promote, data = disc)
disc %>%
  group_by(sex, promote) %>% 
  summarize(n())
#
disc %>%
  group_by(sex) %>% 
  summarize(promoted_prop = mean(promote == "promoted"))

## ------------------------------------------------------------------------
disc %>% 
  table()
# or
xtabs(~sex + promote, data = disc)
prop.table(xtabs(~sex + promote, data = disc), 1)

## ------------------------------------------------------------------------
disc %>%
  group_by(sex) %>% 
  summarize(promoted_prop = mean(promote == "promoted"), promoted_not_prop = mean(promote =="not_promoted"))

## ------------------------------------------------------------------------
disc %>% 
  rep_sample_n(size = nrow(disc), reps = 5)

## ------------------------------------------------------------------------
disc %>% 
  rep_sample_n(size = nrow(disc), reps = 5) %>% 
  mutate(prom_perm = sample(promote)) 

## ------------------------------------------------------------------------
disc %>% 
  rep_sample_n(size = nrow(disc), reps = 5) %>% 
  mutate(prom_perm = sample(promote)) %>% 
  group_by(replicate, sex) %>% 
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted")) 

## ------------------------------------------------------------------------
disc %>% 
  rep_sample_n(size = nrow(disc), reps = 5) %>% 
  mutate(prom_perm = sample(promote)) %>% 
  group_by(replicate, sex) %>% 
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted")) %>% 
  summarize(diff_perm = diff(prop_prom_perm),
            diff_orig = diff(prop_prom))  # male - female

## ------------------------------------------------------------------------
# Create a data frame of differences in promotion rates
disc_perm <- disc %>%
  rep_sample_n(size = nrow(disc), reps = 1000) %>%
  mutate(prom_perm = sample(promote)) %>%
  group_by(replicate, sex) %>%
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted")) %>%
  summarize(diff_perm = diff(prop_prom_perm),
            diff_orig = diff(prop_prom))  # male - female

## ---- label = "gee", fig.cap = "Null distribution"-----------------------
# Histogram of permuted differences
ggplot(disc_perm, aes(x = diff_perm)) + 
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = diff_orig), color = "red", 
             linetype = "dashed") + 
  theme_bw() + 
  labs(x = expression(hat(p)[M]-hat(p)[F]))

## ------------------------------------------------------------------------
disc_perm %>% 
  summarize(q.025 = quantile(diff_perm, p = 0.025),
            q.975 = quantile(diff_perm, p = 0.975))

## ------------------------------------------------------------------------
disc_perm %>% 
  summarize(pvalue = mean(diff_perm >= diff_orig))

## ------------------------------------------------------------------------
disc_perm %>% 
  summarize(q.90 = quantile(diff_perm, p = 0.90))

## ------------------------------------------------------------------------
disc_perm %>% 
  summarize(q.95 = quantile(diff_perm, p = 0.95))

## ------------------------------------------------------------------------
disc_perm %>% 
  summarize(q.99 = quantile(diff_perm, p = 0.99))

## ------------------------------------------------------------------------
disc_perm %>% 
  summarize(q.01 = quantile(diff_perm, p = 0.01))

## ------------------------------------------------------------------------
disc_perm %>% 
  summarize(q.05 = quantile(diff_perm, p = 0.05))

## ------------------------------------------------------------------------
disc_perm %>% 
  summarize(q.10 = quantile(diff_perm, p = 0.10))

## ---- echo = FALSE, label = "Fisher"-------------------------------------
knitr::include_graphics("http://www.swlearning.com/quant/kohler/stat/biographical_sketches/Fisher_3.jpeg")

## ---- echo = FALSE-------------------------------------------------------
GD <- matrix(data = c(3, 5, 1, 7), nrow = 2, byrow = TRUE)
dimnames(GD) <- list(sex = c("male", "female"), promote = c("promoted", "not_promoted"))
GDT <- as.table(GD)
GDTDF <- as.data.frame(GDT)
disc_small <- as.tbl(vcdExtra::expand.dft(GDTDF))
GD <- matrix(data = c(100, 140, 30, 210), nrow = 2, byrow = TRUE)
dimnames(GD) <- list(sex = c("male", "female"), promote = c("promoted", "not_promoted"))
GDT <- as.table(GD)
GDTDF <- as.data.frame(GDT)
disc_big <- as.tbl(vcdExtra::expand.dft(GDTDF))
#
disc_big_perm <- disc_big %>%
  rep_sample_n(size = nrow(disc_big), reps = 1000) %>%
  mutate(prom_perm = sample(promote)) %>%
  group_by(replicate, sex) %>%
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted")) %>%
  summarize(diff_perm = diff(prop_prom_perm),
            diff_orig = diff(prop_prom))  # male - female
disc_small_perm <- disc_small %>%
  rep_sample_n(size = nrow(disc_small), reps = 1000) %>%
  mutate(prom_perm = sample(promote)) %>%
  group_by(replicate, sex) %>%
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted")) %>%
  summarize(diff_perm = diff(prop_prom_perm),
            diff_orig = diff(prop_prom))  # male - female

## ------------------------------------------------------------------------
# Tabulate the small and big data frames
disc_small %>% 
  select(sex, promote) %>%
  table()
disc_big %>% 
  select(sex, promote) %>%
  table()
# Or
table(disc_small)
table(disc_big)
# Or
xtabs(~sex + promote, data = disc_small)
xtabs(~sex + promote, data = disc_big)

## ------------------------------------------------------------------------
# Plot the distributions of permuted differences
ggplot(disc_small_perm, aes(x = diff_perm)) + 
  geom_histogram(binwidth = 0.01, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = diff_orig), 
             color = "red", linetype = "dashed") + 
  theme_bw() + 
  labs(x = expression(hat(p)[M]-hat(p)[F]))
#
ggplot(disc_big_perm, aes(x = diff_perm)) + 
  geom_histogram(binwidth = 0.01, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = diff_orig), 
             color = "red", linetype = "dashed") + 
  theme_bw() + 
  labs(x = expression(hat(p)[M]-hat(p)[F]))

## ------------------------------------------------------------------------
disc_perm %>% 
  summarize(q.90 = quantile(diff_perm, p = 0.90),
            q.95 = quantile(diff_perm, p = 0.95),
            q.99 = quantile(diff_perm, p = 0.99)
            )

## ------------------------------------------------------------------------
disc_small_perm %>% 
  summarize(q.90 = quantile(diff_perm, p = 0.90),
            q.95 = quantile(diff_perm, p = 0.95),
            q.99 = quantile(diff_perm, p = 0.99)
            )

## ------------------------------------------------------------------------
disc_big_perm %>% 
  summarize(q.90 = quantile(diff_perm, p = 0.90),
            q.95 = quantile(diff_perm, p = 0.95),
            q.99 = quantile(diff_perm, p = 0.99)
            )

## ------------------------------------------------------------------------
# Calculate the p-value for the original dataset
disc_perm %>%
  summarize(mean(diff_orig <= diff_perm))
disc_perm %>%
  summarize(mean(diff_perm >= diff_orig))

## ------------------------------------------------------------------------
disc_small_perm %>%
  summarize(mean(diff_perm >= diff_orig))
#
disc_big_perm %>%
  summarize(mean(diff_perm >= diff_orig))

## ------------------------------------------------------------------------
disc_big_perm %>%
  summarize((sum(diff_perm >= diff_orig) + 1)/(1000 + 1))

## ---- echo = FALSE-------------------------------------------------------
GD <- matrix(data = c(7, 17, 6, 18), nrow = 2, byrow = TRUE)
dimnames(GD) <- list(sex = c("female", "male"), promote = c("not_promoted", "promoted"))
GDT <- as.table(GD)
GDTDF <- as.data.frame(GDT)
disc_new <- as.tbl(vcdExtra::expand.dft(GDTDF))
# Create a data frame of differences in promotion rates
disc_new_perm <- disc_new %>%
  rep_sample_n(size = nrow(disc), reps = 1000) %>%
  mutate(prom_perm = sample(promote)) %>%
  group_by(replicate, sex) %>%
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted")) %>%
  summarize(diff_perm = diff(prop_prom_perm),
            diff_orig = diff(prop_prom))  # male - female

## ------------------------------------------------------------------------
# Recall the original data
disc %>% 
  select(sex, promote) %>%
  table()

# Tabulate the new data
disc_new %>% 
  select(sex, promote) %>%
  table()

## ------------------------------------------------------------------------
# Plot the distribution of the original permuted differences
ggplot(disc_perm, aes(x = diff_perm)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = diff_orig), color = "red") + 
  theme_bw() + 
  labs(x = expression(hat(p)[M]-hat(p)[F]))
# Plot the distribution of the new permuted differences
ggplot(disc_new_perm, aes(x = diff_perm)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = diff_orig), color = "red") + 
  theme_bw() + 
  labs(x = expression(hat(p)[M]-hat(p)[F]))

## ------------------------------------------------------------------------
# Find the p-value from the original data
disc_perm %>%
  summarize(mean(diff_perm >= diff_orig))

# Find the p-value from the new data
disc_new_perm %>%
  summarize(mean(diff_perm >= diff_orig))

## ------------------------------------------------------------------------
# Calculate the two-sided p-value
disc_perm %>%
  summarize(mean(diff_perm >= diff_orig)*2)

## ---- echo = FALSE-------------------------------------------------------
GD <- matrix(data = c(56, 41, 19, 34), nrow = 2, byrow = TRUE)
dimnames(GD) <- list(decision = c("buyDVD", "nobuyDVD"), group = c("control", "treatment"))
GDT <- as.table(GD)
GDTDF <- as.data.frame(GDT)
opportunity <- vcdExtra::expand.dft(GDTDF)

## ------------------------------------------------------------------------
# Tabulate the data
opportunity %>%
  select(decision, group) %>%
  table()

## ------------------------------------------------------------------------
# Find the proportion who bought the DVD in each group
opportunity %>%
  group_by(group) %>%
  summarize(buy_prop = mean(decision == "buyDVD"),
            save_pro = mean(decision == "nobuyDVD"))

## ---- label = "bp1", fig.cap = "Spenders and Savers"---------------------
ggplot(data = opportunity, aes(x = group, fill = decision)) +
  geom_bar(position = "fill") + 
  labs(y = "Fraction") + 
  theme_bw() + 
  scale_fill_manual(values = c("red", "green"))

## ------------------------------------------------------------------------
# Data frame of differences in purchase rates after permuting
opp_perm <- opportunity %>%
  rep_sample_n(size = nrow(opportunity), reps = 1000) %>%
  mutate(dec_perm = sample(decision)) %>%
  group_by(replicate, group) %>%
  summarize(prop_buy_perm = mean(dec_perm == "buyDVD"),
            prop_buy = mean(decision == "buyDVD")) %>%
  summarize(diff_perm = diff(prop_buy_perm),
            diff_orig = diff(prop_buy))  # treatment - control

## ---- label = "spend", fig.cap = "Null distribution for proportion of spending differences between the treatment and control groups"----
# Histogram of permuted differences
ggplot(opp_perm, aes(x = diff_perm)) + 
  geom_histogram(binwidth = .03, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = diff_orig), 
             col = "red", linetype = "dashed") + 
  theme_bw() +
  labs(x = expression(hat(p)[buy_treat] - hat(p)[buy_contr]))

## ------------------------------------------------------------------------
PA <- opp_perm %>%
  summarize(pvalue = mean(diff_perm <= diff_orig))
PA
PA$pvalue

## ---- eval = FALSE-------------------------------------------------------
## opp_perm %>%
##   summarize(pvalue = mean(diff_perm <= diff_orig))

## ---- echo = FALSE-------------------------------------------------------
knitr::include_graphics("HTE.png")

## ---- echo = FALSE-------------------------------------------------------
knitr::include_graphics("JE.png")

## ------------------------------------------------------------------------
opp_perm %>%
  summarize(pvalue = mean(diff_perm <= diff_orig)*2)

## ---- echo = FALSE-------------------------------------------------------
knitr::include_graphics("boot.png")

## ---- echo = FALSE-------------------------------------------------------
load("all_polls.RData")

## ------------------------------------------------------------------------
str(all_polls)

## ------------------------------------------------------------------------
one_poll <- all_polls %>% 
  filter(poll == 1)

## ------------------------------------------------------------------------
one_poll %>% 
  summarize(phat = mean(vote))
one_poll_boot_30 <- one_poll %>% 
  rep_sample_n(size = 30, replace = TRUE, reps = 1000)

## ------------------------------------------------------------------------
ex1_props <- all_polls %>% 
  group_by(poll) %>% 
  summarize(prop_yes = mean(vote))

## ------------------------------------------------------------------------
ex2_props <- one_poll_boot_30 %>% 
  summarize(prop_yes = mean(vote))

## ------------------------------------------------------------------------
ex1_props %>% 
  summarize(SE = sd(prop_yes))
ex2_props %>% 
  summarize(SE = sd(prop_yes))

## ------------------------------------------------------------------------
# Experiment 1
sims <- 10^4 - 1
phat1 <- numeric(sims)
for(i in 1:sims){
  phat1[i] <- mean(rbinom(30, 1, 0.6))
}
c(mean(phat1), sd(phat1))
# Experiment 2
set.seed(39)
samp1 <- rbinom(30, 1, 0.6)
mean(samp1)
bsphat2 <- numeric(sims)
for(i in 1: sims){
  bsphat2[i] <- mean(sample(samp1, size = 30, replace = TRUE))
}
c(mean(bsphat2), sd(bsphat2))

## ------------------------------------------------------------------------
one_poll_boot_3 <- one_poll %>% 
  rep_sample_n(size = 3, replace = TRUE, reps = 1000)

## ------------------------------------------------------------------------
one_poll_boot_300 <- one_poll %>% 
  rep_sample_n(size = 300, replace = TRUE, reps = 1000)

## ------------------------------------------------------------------------
ex3_props <- one_poll_boot_3 %>% 
  group_by(replicate) %>%
  summarize(prop_yes = mean(vote))
ex4_props <- one_poll_boot_300 %>% 
  group_by(replicate) %>%
  summarize(prop_yes = mean(vote))

## ------------------------------------------------------------------------
ex3_props %>% 
  summarize(sd(prop_yes))
ex4_props %>% 
  summarize(sd(prop_yes))

## ------------------------------------------------------------------------
sims <- 10^4 - 1
bsphat3 <- numeric(sims)
bsphat4 <- numeric(sims)
set.seed(39)
samp1 <- rbinom(30, 1, 0.6)
mean(samp1)
for(i in 1:sims){
  bsphat3[i] <- mean(sample(samp1, size = 3, replace = TRUE))
  bsphat4[i] <- mean(sample(samp1, size = 300, replace = TRUE))
}
c(mean(bsphat3), sd(bsphat3), mean(bsphat4), sd(bsphat4))

## ------------------------------------------------------------------------
# Recall the variability of sample proportions
ex1_props %>% summarize(sd(prop_yes))
ex2_props %>% summarize(sd(prop_yes))
ex3_props %>% summarize(sd(prop_yes))
ex4_props %>% summarize(sd(prop_yes))

## ------------------------------------------------------------------------
# Create smoothed density curves for all four experiments
ggplot() + 
  geom_density(data = ex1_props, aes(x = prop_yes), color = "black", bw = .1) +
  geom_density(data = ex2_props, aes(x = prop_yes), color = "green", bw = .1) +
  geom_density(data = ex3_props, aes(x = prop_yes), color = "red", bw = .1) +
  geom_density(data = ex4_props, aes(x = prop_yes), color = "blue", bw = .1) + 
  theme_bw()

## ------------------------------------------------------------------------
df <- data.frame(p1 = phat1, p2 = bsphat2, p3 = bsphat3, p4 = bsphat4)
ggplot(data = df) + 
  geom_density(aes(x = p1), color = "black", bw = 0.1) + 
  geom_density(aes(x = p2), color = "green", bw = 0.1) + 
  geom_density(aes(x = p3), color = "red", bw = 0.1) + 
  geom_density(aes(x = p4), color = "blue", bw = 0.1) + 
  theme_bw() + 
  labs(x = substitute(paste(hat(p),"*")))

## ------------------------------------------------------------------------
# Compute proportion of votes for Candidate X: props
props <- all_polls %>%
  group_by(poll) %>% 
  summarize(prop_yes = mean(vote))

## ------------------------------------------------------------------------
# Proportion of polls within 2SE
props %>%
  mutate(lower = mean(prop_yes) - 2 * sd(prop_yes),
         upper = mean(prop_yes) + 2 * sd(prop_yes),
         in_CI = prop_yes > lower & prop_yes < upper) %>%
  summarize(mean(in_CI))

## ------------------------------------------------------------------------
mphat1 <- mean(phat1)
se <- sd(phat1)
c(mphat1, se)
LCI <- mphat1 + qnorm(0.025)*se
UCI <- mphat1 + qnorm(0.975)*se
c(LCI, UCI)
mean(phat1 > LCI & phat1 < UCI)

## ------------------------------------------------------------------------
# Again, set the one sample that was collected
one_poll <- all_polls %>%
  filter(poll == 1) %>%
  select(vote)
# Compute p-hat from one_poll: p_hat
p_hat <- mean(one_poll$vote)

## ------------------------------------------------------------------------
# Bootstrap to find the SE of p-hat: one_poll_boot
one_poll_boot <- one_poll %>%
  rep_sample_n(30, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote))

## ------------------------------------------------------------------------
# Create an interval of plausible values
one_poll_boot %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot))
# or
p_hat +c(-1, 1)*2*sd(bsphat2)

## ------------------------------------------------------------------------
# Find the 2.5% and 97.5% of the p-hat values
one_poll_boot %>% 
  summarize(q025_prop = quantile(prop_yes_boot, probs = 0.025),
            q975_prop = quantile(prop_yes_boot, probs = 0.975))
# or
quantile(bsphat2, probs = c(0.025, 0.975))

## ------------------------------------------------------------------------
# Bootstrap t-confidence interval for comparison
one_poll_boot %>%
  summarize(lower = p_hat - 2*sd(prop_yes_boot),
            upper = p_hat + 2*sd(prop_yes_boot))

## ------------------------------------------------------------------------
# Recall the bootstrap t-confidence interval
p_hat <- mean(one_poll$vote)
one_poll_boot %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot))

## ------------------------------------------------------------------------
# Collect a sample of 30 observations from the population
set.seed(13)
one_poll <- as.tbl(data.frame(vote = rbinom(n = 30, 1, .6)))

## ------------------------------------------------------------------------
# Resample the data using samples of size 300 (an incorrect strategy!)
one_poll_boot_300 <- one_poll %>%
  rep_sample_n(300, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote))

## ------------------------------------------------------------------------
# Find the endpoints of the the bootstrap t-confidence interval
one_poll_boot_300 %>%
  summarize(lower = p_hat - 2*sd(prop_yes_boot),
            upper = p_hat + 2*sd(prop_yes_boot))

## ------------------------------------------------------------------------
# Resample the data using samples of size 3 (an incorrect strategy!)
one_poll_boot_3 <- one_poll %>%
  rep_sample_n(3, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote)) 

## ------------------------------------------------------------------------
# Find the endpoints of the the bootstrap t-confidence interval 
one_poll_boot_3 %>%
  summarize(lower = p_hat - 2*sd(prop_yes_boot),
            upper = p_hat + 2*sd(prop_yes_boot))

## ------------------------------------------------------------------------
# Collect 30 observations from a population with true proportion of 0.8
set.seed(13)
one_poll <- as.tbl(data.frame(vote = rbinom(n = 30, size = 1, prob = 0.8)))

## ------------------------------------------------------------------------
p_hat <- mean(one_poll$vote)
p_hat

## ------------------------------------------------------------------------
# Resample the 30 observations (with replacement)
one_poll_boot <- one_poll %>%
  rep_sample_n(30, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote)) 

## ------------------------------------------------------------------------
# Calculate the bootstrap t-confidence interval
one_poll_boot %>%
  summarize(lower = p_hat - 2*sd(prop_yes_boot),
            upper = p_hat + 2*sd(prop_yes_boot))

## ------------------------------------------------------------------------
# Calculate a 95% bootstrap percentile interval
one_poll_boot %>% 
  summarize(q025_prop = quantile(prop_yes_boot, probs = 0.025),
            q975_prop = quantile(prop_yes_boot, probs = 0.975))

## ------------------------------------------------------------------------
# Calculate a 99% bootstrap percentile interval
one_poll_boot %>% 
  summarize(q005_prop = quantile(prop_yes_boot, probs = 0.005),
            q995_prop = quantile(prop_yes_boot, probs = 0.995))

## ------------------------------------------------------------------------
# Calculate a 90% bootstrap percentile interval
one_poll_boot %>% 
  summarize(q05_prop = quantile(prop_yes_boot, probs = 0.05),
            q95_prop = quantile(prop_yes_boot, probs = 0.95))

