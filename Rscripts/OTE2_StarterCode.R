## Pre test Code 

library(infer)
library(resampledata)
library(tidyverse)
fullterm <- TXBirths2004 %>% 
  filter(MothersAge == "25-29", Gestation >= 38, 
         Gestation <= 42, Multiple == "No")

## ------------------------------------------------------------------------
set.seed(49)
library(infer)
bdis <- fullterm %>% 
specify(response = Weight) %>% 
generate(reps = 1000, type = "bootstrap") %>% 
calculate(stat = "mean")


set.seed(39)
dd <- fullterm %>% 
  specify(Weight ~ Smoker) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "diff in means", order = c("No", "Yes"))
visualize(dd)
CI <- conf_int(dd, level = 0.90)
CI

set.seed(19)
null <- fullterm %>% 
  specify(Weight ~ Smoker) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("No", "Yes"))
pval <- p_value(null, obs_stat = 196.924, direction = "right")
pval


ftag <- TXBirths2004 %>% 
  filter(Gestation >= 38, Gestation <= 42, Multiple == "No", 
         MothersAge != "under 15", MothersAge != "40-44") %>% 
  droplevels()
mod.aov <- lm(Weight ~ MothersAge, data = ftag)
anova(mod.aov)


set.seed(9)
pF <- ftag %>% 
  specify(Weight ~ MothersAge) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "F")
pval <- get_pvalue(pF, 6.7592, direction = "right")
pval

