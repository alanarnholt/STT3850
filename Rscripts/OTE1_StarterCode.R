# OTE1 --- Starter Code

# 11
library(ggplot2)
library(dplyr)
set.seed(113)
X <- rnorm(1000, 10, 3) + runif(1000, 7, 13)
DF <- data.frame(x = X)


# 12
library(dplyr)
library(resampledata)
TBL <- Verizon %>% 
  group_by(_____) %>% 
  summarize(Mean = mean(_______), n())
TBL
obs_diff <- TBL$Mean[___] - TBL$Mean[___]
obs_diff
Times <- Verizon$Time
set.seed(413)
sims <- 10^4 - 1
ans <- numeric(sims)
for(i in ________){
  index <- sample(1664 + ____, ____)
  ans[i] <- mean(Times[________]) - mean(Times[________])
}
pvalue <- (sum(ans _____ obs_diff) + 1)/(sims + 1)
pvalue