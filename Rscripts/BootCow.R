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


