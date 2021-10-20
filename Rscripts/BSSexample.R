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
# 2 independent populations
#######################################################################
mythbusters_yawn
mythbusters_yawn %>% 
  count(group, yawn)

### Compute obs prop diff

# With infer
mythbusters_yawn %>% 
  specify(yawn ~ group, success = "yes") %>% 
  calculate(stat = "diff in props", order = c("seed", "control")) -> obs_diff2
obs_diff2

## Splitting out the two independent samples
mythbusters_yawn %>% 
  filter(group == "seed") -> gs
mythbusters_yawn %>% 
  filter(group == "control") -> gc
(n1 <- length(gs$yawn))
(n2 <- length(gc$yawn))

(phatseed <- mean(gs$yawn == "yes"))
(phatcontrol <- mean(gc$yawn == "yes"))
(obs_diff3 <- phatseed - phatcontrol)

####
#### Using tapply

tapply(mythbusters_yawn$yawn == "yes", mythbusters_yawn$group, mean)
obs_diff4 <- diff(tapply(mythbusters_yawn$yawn == "yes", mythbusters_yawn$group, mean))
obs_diff4

#### Using xtabs
T1 <- xtabs(~yawn+group, data = mythbusters_yawn)
T1
prop.table(T1, 2)
T1
prop.table(T1, 2)[2,]
obs_diff <- diff(prop.table(T1, 2)[2,])  # phat_seed - phat_control
obs_diff


#####
# Using indexing to split two sample

(gss <- mythbusters_yawn$yawn[mythbusters_yawn$group == "seed"])
(gcs <- mythbusters_yawn$yawn[mythbusters_yawn$group == "control"])
(ps <- mean(gss == "yes"))
(pc <- mean(gcs == "yes"))

# Or using subset()
(gss1 <- subset(mythbusters_yawn, subset = group == "seed", select = yawn, drop = TRUE))
(gcs1 <- subset(mythbusters_yawn, subset = group == "control", select = yawn, drop = TRUE))

##########################

B <- 10000
diffp <- numeric(B)
for(i in 1:B){
  bss1 <- sample(gss1, size = n1, replace = TRUE)
  bss2 <- sample(gcs1, size = n2, replace = TRUE)
  diffp[i] <- mean(bss1=="yes") - mean(bss2=="yes")
}
hist(diffp)
abline(v = 0, col = "blue", lwd = 3)
CIPI_l <- quantile(diffp, probs = c(0.025, 0.975))
CIPI_l

CISE_l <- obs_diff + c(-1,1)*qnorm(.975)*sd(diffp)
CISE_l

######## Using infer now

##### Develop CI for p_seed - p_control

mythbusters_yawn %>% 
  specify(yawn ~ group, success = "yes") %>% 
  generate(reps = 10000, type = "bootstrap") %>% 
  calculate(stat = "diff in props", order = c("seed", "control")) -> BSD
visualize(BSD)  
get_confidence_interval(BSD, level = 0.95) -> CIPI
CIPI
CIPI_l

visualize(BSD) + shade_confidence_interval(endpoints = CI) + 
  geom_vline(xintercept = 0, color = "red", size = 2)

get_confidence_interval(BSD, level = 0.95, point_estimate = obs_diff, type = "se") -> CISE
CISE
CISE_l


############################

