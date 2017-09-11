# Simpson's Paradox
# Alan T. Arnholt
str(UCBAdmissions)
UCBAdmissions
# Convert to DF
UCB <- as.data.frame(UCBAdmissions)
UCB
# Not quite what we want yet...
library(dplyr)
ucb_admit <- UCB %>% 
  slice(rep(1:n(), Freq)) %>% 
  select(-Freq)
ucb_admit
# ucb_admit is what I want!
ucb_admit %>%
  count(Admit, Gender)
# Does this look like discrimination?
library(ggplot2)
ggplot(data = ucb_admit, aes(x = Gender, fill = Admit)) + 
  geom_bar(position = "dodge")
#
ggplot(data = ucb_admit, aes(x = Gender, fill = Admit)) + 
  geom_bar(position = "fill") + 
  labs(y = "Fraction") +
  scale_fill_manual(values = c("green", "red"))
#
library(tidyr)
ucb_admit %>%
  count(Admit, Gender) %>%
  spread(Admit, n)
#
ucb_admit %>%
  count(Admit, Gender) %>%
  spread(Admit, n) %>%
  mutate(Percent_Admit = Admitted/(Admitted + Rejected), 
         Percent_Rejected = Rejected/(Admitted + Rejected))
# Store the results
basicUCB <- ucb_admit %>%
  count(Admit, Gender) %>%
  spread(Admit, n) %>%
  mutate(Percent_Admit = Admitted/(Admitted + Rejected), 
         Percent_Rejected = Rejected/(Admitted + Rejected))
basicUCB
# Graph
library(ggplot2)
ggplot(data = basicUCB, aes(x = Gender, y = Percent_Admit)) + 
  geom_bar(stat = "identity", fill = c("blue","pink")) + 
  theme_bw()

ucb_admit %>%
  group_by(Dept) %>%
  count(Admit, Gender) %>%
  spread(Admit, n) %>%
  mutate(Percent_Admit = Admitted/(Admitted + Rejected), 
         Percent_Rejected = Rejected/(Admitted + Rejected))
# Store results
UCBres <- ucb_admit %>%
  group_by(Dept) %>%
  count(Admit, Gender) %>%
  spread(Admit, n) %>%
  mutate(Percent_Admit = Admitted/(Admitted + Rejected), 
         Percent_Rejected = Rejected/(Admitted + Rejected))
UCBres
# Graph now
ggplot(data = UCBres, aes(x = Dept, y = Percent_Admit, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(y = "Percent Admitted") + 
  theme_bw() + 
  scale_fill_manual(values = c("blue", "pink"))
#
ggplot(data = UCBres, aes(x = Dept, y = Percent_Rejected, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(y = "Percent Rejected") + 
  theme_bw() + 
  scale_fill_manual(values = c("blue", "pink"))
#
# Mosaic Plots
library(vcd)
mosaic(~Gender, data = UCB)
mosaic(~Gender + Admit, data = UCB)
mosaic(~Gender + Admit, data = UCB, shade = TRUE)
mosaic(~Dept + Gender + Admit, data = UCB, shade = TRUE)
mosaic(~Dept + Gender + Admit, data = UCB, shade = TRUE, 
       direction = c("v", "h", "v"))
mosaic(~Dept + Gender + Admit, data = UCB, shade = TRUE, 
       direction = c("v", "h", "v"), highlighting = "Admit")
###
View(UCB)
