age <- c(37, 23, 27, 31, 26, 21, 20, 18, 19, 21)
(xbar <- mean(age))
(S <- sd(age))
CT <- qt(.95, 9)
CT
xbar +c(-1,1)*CT*S/sqrt(10)

t.test(age, conf.level = .90)$conf
t.test(age, mu = 23, alternative = "greater")
hist(age)


B <- 10000
xbarstar <- numeric(B)
for(i in 1:B){
  bss <- sample(age, size = 10, replace = TRUE)
  xbarstar[i] <- quantile(bss, probs = .25)
}
hist(xbarstar)
quantile(xbarstar, probs = c(.05, 0.95))

library(tidyverse)
library(infer)

DF <- data.frame(age = age)
head(DF)
DF %>%
  specify(response = age) %>%
  generate(reps = 10000, type = "bootstrap") %>%
  calculate(stat = "mean") -> bsd 
visualize(bsd)
get_confidence_interval(bsd, level = .90)
qt(.80, 34)
