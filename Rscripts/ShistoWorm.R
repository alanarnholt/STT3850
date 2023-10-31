worms <- c(7, 10, 10, 16, 17, 1, 2, 2, 7, 10)
group <- c(rep("Control", 5), rep("Drug", 5))
DF <- data.frame(worms = worms, group = group)
DF
rm(worms, group)
DF
library(tidyverse)
DF %>% 
  group_by(group) %>% 
  summarize(M = mean(worms))

tapply(DF$worms, DF$group, mean)
(-diff(tapply(DF$worms, DF$group, mean)) -> obs_diff) 
# Do repeatedly 
-diff(tapply(DF$worms, sample(DF$group), mean))


B <- 10^4
mwd <- numeric(B)
for(i in 1:B){
  mwd[i] <- -diff(tapply(DF$worms, sample(DF$group), mean))
}

hist(mwd)
sum(mwd >= 7.6)
(pv <- sum(mwd >= 7.6)/B)

