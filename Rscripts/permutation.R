worms <-c(17, 16, 10, 10, 7, 10, 7, 2, 2, 1)
treat <- c(rep("no", 5), rep("yes", 5))
ic <- tibble(worms, treat)

ic
library(tidyverse)
ic |> 
  group_by(treat) |> 
  summarize(AW = mean(worms)) -> tab
tab
obs_diff <- tab$AW[1] - tab$AW[2]
obs_diff

set.seed(321)
P <- 10^4
md <- numeric(P)
for(i in 1:P){
  md[i] <- -diff(tapply(worms, sample(treat), mean))
}
hist(md)

(pvalue <- mean(md >= obs_diff))
