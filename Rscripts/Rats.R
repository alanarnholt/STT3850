Worms <- c(17, 16, 10, 10, 7, 10, 7, 2, 2, 1)
Group <- c(rep("Control", 5), rep("Treatment", 5))
DF <- data.frame(Worms, Group)
DF








tapply(DF$Worms, DF$Group, mean)
diff(tapply(DF$Worms, DF$Group, mean))




B <- 10^4
md <- numeric(B)
for(i in 1:B){
  md[i] <- diff(tapply(DF$Worms, sample(DF$Group), mean))
}


B <- 10^4
md <- numeric(B)
for(i in 1:B){
  index <- sample(1:10, 5, replace = FALSE)
  md[i] <- mean(Worms[index]) - mean(Worms[-index])
}
hist(md)



library(tidyverse)
library(infer)
library(moderndive)
pennies_sample
(ob_stat <- mean(pennies_sample$year))
# Want to test null mu = 1993 since xbar is 1995.44 will 
# need to subtract 2.44 from each value to make the null true.
B <- 10^4
m <- numeric(B)
for(i in 1:B){
  bss <- sample(pennies_sample$year, size = 50, replace = TRUE) - 2.44
  m[i] <- mean(bss)
}
hist(m)
(pvalue <- mean(m >= ob_stat)*2)



pennies_sample %>% 
  specify(response = year) %>% 
  hypothesize(null = "point", mu = 1993) %>% 
  generate(reps = 10000, type = "bootstrap") %>% 
  calculate(stat = "mean") -> null_dist
visualize(null_dist)
get_pvalue(null_dist, obs_stat = ob_stat, direction = "both")
  