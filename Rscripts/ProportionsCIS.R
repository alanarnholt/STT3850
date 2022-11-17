# 
library(resampledata)
library(tidyverse)
junk <- TXBirths2004
head(junk)
junk %>%
  count(Smoker)
table(junk$Smoker)
xtabs(~Smoker, data = junk)
(z <- qnorm(.97))
phat <- 90/(1497+90)
phat
phat + c(-1, 1)*z*sqrt(phat*(1-phat)/(1497+90))

prop.test(x = 90, n = 90+1497, correct = FALSE, conf.level = 0.94)$conf

library(infer)
junk %>%
  specify(response = Smoker, success = "Yes") %>%
  generate(reps = 10^4, type = "bootstrap") %>%
  calculate(stat = "prop") -> pdist
get_confidence_interval(pdist, level = 0.94)
########################################################
########################################################
library(resampledata)
library(tidyverse)
junk <- TXBirths2004
B <- 10^4
phat <- numeric(B)
for(i in 1:B){
  bss <- sample(junk$Smoker, size = sum(!is.na(junk$Smoker)), replace = TRUE)
  phat[i] <- mean(bss == "Yes")
}
hist(phat)
CI <- round(quantile(phat, probs = c(0.03, 0.97)),4)
CI
