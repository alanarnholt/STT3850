library(resampledata)
Bangladesh
B <- 10000
Bang <- Bangladesh
cobalt <- Bang$Cobalt[-252]
summary(cobalt)
bs_sd <- numeric(B)
for(i in 1:B){
  bss <- sample(cobalt, size = 270, replace = TRUE)
  bs_sd[i] <- sd(bss)
}
hist(bs_sd)

quantile(bs_sd, probs = c(.05, 0.95))

library(infer)
library(tidyverse)
Bang |> 
  specify(response = Cobalt) |> 
  generate(reps = B, type = "bootstrap") |> 
  calculate(stat = "sd") -> bs_dist
visualize(bs_dist)
get_confidence_interval(bs_dist, level = .90)
