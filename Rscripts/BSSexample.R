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
  generate(reps = 1000, type = "bootstrap") %>% 
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

N <- 1000
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