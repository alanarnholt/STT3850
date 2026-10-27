# Class 10/27/2026

hours <-c(14, 15, 15, 15, 14, 13, 8, 
          17, 15, 11, 18, 18, 14, 15)
# 83% Ci for Mu
set.seed(123)
B <- 10000
bxbar <- numeric(B)
for(i in 1:B){
  bss <- sample(hours, size = 14, replace = TRUE)
  bxbar[i] <- mean(bss)
}
hist(bxbar)
mean(hours)
mean(bxbar)
quantile(bxbar, probs = c(0.085, 1 - 0.085))
library(infer)
library(tidyverse)
DF <- data.frame(hours)
DF

DF |> 
  specify(response = hours) |> 
  generate(reps = B, type = "bootstrap") |> 
  calculate(stat = "mean") -> BSD
BSD
ggplot(data = BSD, aes(x = stat)) +
  geom_histogram()
visualize(BSD)
BSD |> 
  summarize(LEP = quantile(stat, .085),
            UEP = quantile(stat, 1 - 0.085))
get_confidence_interval(BSD, level = .83)
t.test(hours, conf.level = .83)$conf -> CI
CI[1]
CI[2]
