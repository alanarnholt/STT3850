# In Class Problem

library(moderndive)
library(infer)
library(tidyverse)

head(bowl)

bowl %>% 
  summarize(phat = mean(color == "red"))

bowl_sample_1

#### Bootstrap CI

bowl_sample_1 %>% 
  specify(response = color, success = "red") %>% 
  generate(reps = 10000, type = "bootstrap") %>% 
  calculate(stat = "prop") -> bsd1
bsd1

visualize(bsd1)

CI <- get_confidence_interval(bsd1)
CI

visualize(bsd1) + shade_confidence_interval(endpoints = CI)

###
bowl_sample_1 %>% 
  summarize(phat = mean(color == "red")) %>% 
  pull() -> ans
ans

###

### SE CI

bsd1 %>% 
  summarize(SE = sd(stat),
            lep = ans -2*SE,
            uep = ans + 2*SE)

############ Using a loop

N <- 10000
phat <- numeric(N)
for(i in 1:N){
  bss <- sample(bowl_sample_1$color, size = 50, replace = TRUE)
  phat[i] <- mean(bss == "red")
}
hist(phat)

CIPI <- quantile(phat, probs = c(0.025, 0.975))
CIPI

###
(p1 <- mean(bowl_sample_1$color == "red"))
CISE <- p1 + c(-1,1)*2*sd(phat)
CISE

#######################################################################
mythbusters_yawn
mythbusters_yawn %>% 
  count(group, yawn)
T1 <- xtabs(~yawn+group, data = mythbusters_yawn)
T1
prop.table(T1, 2)
T1
obs_diff <- diff(prop.table(T1, 2)[2,])
obs_diff

mythbusters_yawn %>% 
  filter(group == "seed") -> g1
mythbusters_yawn %>% 
  filter(group == "control") -> g2
(n1 <- length(g1$yawn))
(n2 <- length(g2$yawn))

B <- 10000
diffp <- numeric(B)
for(i in 1:B){
  bss1 <- sample(g1$yawn, size = n1, replace = TRUE)
  bss2 <- sample(g2$yawn, size = n2, replace = TRUE)
  diffp[i] <- mean(bss1=="yes") - mean(bss2=="yes")
}
hist(diffp)
CIPI <- quantile(diffp, probs = c(0.025, 0.975))
CIPI

######## Using infer now

mythbusters_yawn %>% 
  specify(yawn ~ group, success = "yes") %>% 
  generate(reps = 10000, type = "bootstrap") %>% 
  calculate(stat = "diff in props", order = c("seed", "control")) -> BSD
visualize(BSD)  
get_confidence_interval(BSD, level = 0.95) -> CI
CI
visualize(BSD) + shade_confidence_interval(endpoints = CI)

get_confidence_interval(BSD, level = 0.95, point_estimate = obs_diff, type = "se") -> CISE
CISE
CI
