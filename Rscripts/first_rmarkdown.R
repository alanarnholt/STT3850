## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = NA, fig.align = "center", message = FALSE)

## ------------------------------------------------------------------------
summary(UCBAdmissions)
str(UCBAdmissions)

## ------------------------------------------------------------------------
UCB <- as.data.frame(UCBAdmissions)
UCB
# Not quite what we want yet
library(dplyr)
ucb_admit <- UCB %>% 
  slice(rep(1:n(), Freq)) %>% 
  select(-Freq)
ucb_admit

## ------------------------------------------------------------------------
ucb_admit %>%
  count(Admit, Gender)

## ------------------------------------------------------------------------
library(ggplot2)
ggplot(data = ucb_admit, aes(x = Gender, fill = Admit)) + 
  geom_bar(position = "dodge")

## ------------------------------------------------------------------------
ggplot(data = ucb_admit, aes(x = Gender, fill = Admit)) + 
  geom_bar(position = "fill") + 
  labs(y = "Fraction") +
  scale_fill_manual(values = c("green", "red")) + 
  theme_bw()

## ------------------------------------------------------------------------
library(tidyr)
ucb_admit %>%
  count(Admit, Gender) 
ucb_admit %>%
  count(Admit, Gender) %>%
  spread(Admit, n)
ucb_admit %>%
  count(Admit, Gender) %>%
  spread(Admit, n) %>% 
  mutate(Percent_Rejected = Rejected / (Admitted + Rejected),
         Percent_Accepted = Admitted /(Admitted + Rejected))

## ------------------------------------------------------------------------
basicUCB <- ucb_admit %>%
  count(Admit, Gender) %>%
  spread(Admit, n) %>% 
  mutate(Percent_Rejected = Rejected / (Admitted + Rejected),
         Percent_Accepted = Admitted /(Admitted + Rejected))
basicUCB

## ------------------------------------------------------------------------
ggplot(data = basicUCB, aes(x = Gender, y = Percent_Accepted)) + 
  geom_bar(stat = "identity", fill = c("blue", "pink")) + 
  theme_bw()

## ------------------------------------------------------------------------
UCBres <- ucb_admit %>%
  group_by(Dept) %>%
  count(Admit, Gender) %>%
  spread(Admit, n) %>%
  mutate(Percent_Accepted = Admitted/(Admitted + Rejected), 
         Percent_Rejected = Rejected/(Admitted + Rejected))
UCBres

## ------------------------------------------------------------------------
ggplot(data = UCBres, aes(x = Dept, y = Percent_Accepted, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(y = "Percent Admitted", x = "Department") + 
  theme_bw() + 
  scale_fill_manual(values = c("blue", "pink"))

## ------------------------------------------------------------------------
ggplot(data = UCBres, aes(x = Dept, y = Percent_Rejected, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(y = "Percent Rejected", x = "Department") + 
  theme_bw() + 
  scale_fill_manual(values = c("blue", "pink"))

## ------------------------------------------------------------------------
library(vcd)
mosaic(~Gender, data = UCB)
mosaic(~Gender + Admit, data = UCB)
mosaic(~Gender + Admit, data = UCB, shade = TRUE)
mosaic(~Dept + Gender + Admit, data = UCB, shade = TRUE)
mosaic(~Dept + Gender + Admit, data = UCB, shade = TRUE, 
       direction = c("v", "h", "v"))
mosaic(~Dept + Gender + Admit, data = UCB, shade = TRUE, 
       direction = c("v", "h", "v"), highlighting = "Admit")

## ------------------------------------------------------------------------
library(openintro)
data(county)
str(county)
summary(county)

## ------------------------------------------------------------------------
county_noDC <- county %>% 
  filter(state != "District of Columbia") %>% 
  droplevels()
dim(county_noDC)

## ------------------------------------------------------------------------
set.seed(123)
county_srs <- county_noDC %>% 
  sample_n(size = 100)
head(county_srs)

## ------------------------------------------------------------------------
county_srs %>% 
  summarize(Average = mean(fed_spend))

## ------------------------------------------------------------------------
county_srs %>%
  summarize(Number = n_distinct(state))

## ------------------------------------------------------------------------
set.seed(321)
county_str <- county_noDC %>% 
  group_by(state) %>% 
  sample_n(size = 2)
head(county_str)

## ------------------------------------------------------------------------
county_str %>% 
  summarize(Average = mean(fed_spend))

## ------------------------------------------------------------------------
county_str %>% 
  ungroup() %>% # return ungrouped copy of table
  summarize(Average = mean(fed_spend, na.rm = TRUE))

