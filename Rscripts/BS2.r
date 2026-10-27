# Class 1 pm 10/27/2026

hours <- c(13, 14, 16, 13, 11, 14, 14, 16, 14)
B <- 10000
bxbar <- numeric(B)
for(i in 1:B){
  bss <- sample(hours, size = 9, replace = TRUE)
  bxbar[i] <- mean(bss)
}
hist(bxbar)
quantile(bxbar, probs = c(.04, .96)) -> BPCI
BPCI

library(infer)
library(tidyverse)
TB <- tibble(hours)
TB
TB |> 
  specify(response = hours) |> 
  generate(reps = 10000, type = "bootstrap") |> 
  calculate(stat = "mean") -> BSD
BSD
BSD |> 
  summarize(LEP = quantile(stat, 0.04),
            UEP = quantile(stat, 0.96)) -> BPCI2
BPCI2

visualize(BSD)
visualise(BSD)
get_ci(BSD, level = .92)
t.test(hours, conf.level = .92)$conf
