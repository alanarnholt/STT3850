# In Class

library(resampledata)
library(tidyverse)
head(Beerwings)

Beerwings %>% 
  group_by(Gender) %>% 
  summarize(MW = mean(Hotwings), MB = mean(Beer), n = n())


tapply(Beerwings$Hotwings, Beerwings$Gender, mean) -> md
md
diff(md)

# Bootstrap dist of difference in mean wings
library(infer)
Beerwings %>% 
  specify(Hotwings ~ Gender) %>% 
  generate(reps = 10000, type = "bootstrap") %>% 
  calculate(stat = "diff in means", order = c("M", "F")) -> bsd
bsd
visualize(bsd)
get_confidence_interval(bsd)
############################

MW <- Beerwings$Hotwings[Beerwings$Gender == "M"]
MW
FW <- Beerwings$Hotwings[Beerwings$Gender == "F"]
FW
od <- mean(MW) - mean(FW)
od
####
B <- 10000
md <- numeric(B)
for(i in 1:B){
  bss1 <- sample(MW, size = 15, replace = TRUE)
  bss2 <- sample(FW, size = 15, replace = TRUE)
  md[i] <- mean(bss1) - mean(bss2)
}
hist(md)
quantile(md, probs = c(0.025, 0.975))
