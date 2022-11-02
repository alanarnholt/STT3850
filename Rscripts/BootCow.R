# Alan Arnholt
# Class: Bootstrapping: percentile CIs and bootstrap T CIs

# First Examine butterfat versus age
library(PASWR2)
library(tidyfverse)

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


# 95% Bootstrap percentile CI for mu_2yo - mu_mature
set.seed(13)
COWS %>% filter(age == "2 years old") %>% select(butterfat) %>% pull() -> twoyearold_bf
COWS %>% filter(age == "Mature") %>% select(butterfat) %>% pull() -> mature_bf
B <- 10^4
md <- numeric(B)
for(i in 1:B){
  bsstyo <- sample(twoyearold_bf, size = sum(!is.na(twoyearold_bf)), replace = TRUE)
  bssmat <- sample(mature_bf, size = sum(!is.na(mature_bf)), replace = TRUE)
  md[i] <- mean(bsstyo) - mean(bssmat)
}

CI <- quantile(md, probs = c(0.025, 0.975))
CI

# 95% Bootstrap T CI for mu_2yo - mu_mature
set.seed(13)
COWS %>% filter(age == "2 years old") %>% select(butterfat) %>% pull() -> twoyearold_bf
n_tyo <- sum(!is.na(twoyearold_bf))
n_mat <- sum(!is.na(mature_bf))
c(n_tyo, n_mat)
COWS %>% filter(age == "Mature") %>% select(butterfat) %>% pull() -> mature_bf
B <- 10^4
Q <- numeric(B)
for(i in 1:B){
  bsstyo <- sample(twoyearold_bf, size = sum(!is.na(twoyearold_bf)), replace = TRUE)
  bssmat <- sample(mature_bf, size = sum(!is.na(mature_bf)), replace = TRUE)
  Q[i] <- ( (mean(bsstyo) - mean(bssmat)) - (mean(twoyearold_bf) - mean(mature_bf)) ) / sqrt(var(bsstyo)/n_tyo + var(bssmat)/n_mat) 
}

QS <- quantile(Q, probs = c(0.025, 0.975))
QS
CIBT <- c((mean(twoyearold_bf) - mean(mature_bf)) - QS[2]*sqrt(var(twoyearold_bf)/n_tyo + var(mature_bf)/n_mat),
          (mean(twoyearold_bf) - mean(mature_bf)) - QS[1]*sqrt(var(twoyearold_bf)/n_tyo + var(mature_bf)/n_mat)  )
CIBT

# Construct a 95% bootstrap percentile CI for mu_tyo/mu_mat

set.seed(13)
B <- 10^4
rm <- numeric(B)
for(i in 1:B){
  bsstyo <- sample(twoyearold_bf, size = sum(!is.na(twoyearold_bf)), replace = TRUE)
  bssmat <- sample(mature_bf, size = sum(!is.na(mature_bf)), replace = TRUE)
  rm[i] <- mean(bsstyo)/mean(bssmat)
}

CI <- quantile(rm, probs = c(0.025, 0.975))
CI
