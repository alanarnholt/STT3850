####
library(tidyverse)
GSS2002 <- read.csv("http://www1.appstate.edu/~arnholta/Data/GSS2002.csv")
GSS2002 <- tbl_df(GSS2002)
GSS2002
xtabs(~Education + DeathPenalty, data = GSS2002)
xtabs(~Politics + Religion, data = GSS2002)
####

GSS2002 %>% 
  group_by(Education, DeathPenalty) %>% 
  summarize(n()) %>% 
  ggplot(aes(x = Education, y = `n()`, fill = DeathPenalty)) + 
  geom_bar(stat = "identity", position = "fill")


GSS2002 %>% 
  group_by(Education, DeathPenalty) %>% 
  summarize(n()) %>% 
  na.omit() %>% 
  ggplot(aes(x = Education, y = `n()`, fill = DeathPenalty)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_manual(values = c("red", "purple"))


GSSFIX <- GSS2002 %>% 
  select(Education, DeathPenalty) %>% 
  na.omit()
ggplot(data  = GSSFIX, aes(x = Education, fill = DeathPenalty)) + 
  geom_bar(position = "fill")
xtabs(~Education + DeathPenalty, data = GSSFIX)
# A permutation
xtabs(~Education + sample(DeathPenalty), data = GSSFIX)

sims <- 10^4 - 1
chi2 <- numeric(sims)
for(i in 1:sims){
  DT <- xtabs(~Education + sample(DeathPenalty), data = GSSFIX)
  chi2[i] <- chisq.test(DT)$stat
}
DF <- data.frame(x = chi2)
ggplot(data = DF, aes(x = x)) +
  geom_density(fill = "purple") + 
  theme_bw() + 
  stat_function(fun = dchisq, args = list(df = 4), color = "blue", lwd = 2)


xtabs(~ Religion + Politics, data = GSS2002)

GSS2002 %>% 
  group_by(Religion, Politics) %>%
  filter(Religion == "Catholic" | Religion == "None" | Religion == "Protestant" | Religion == "Moslem/Islam") %>% 
  summarize(n()) %>% 
  na.omit() %>% 
  ggplot(aes(x = Religion, y = `n()`, fill = Politics)) + 
  geom_bar(stat = "identity", position = "fill")

GSS2002$Politics <- factor(GSS2002$Politics, levels = c("Extremely liberal", "Slightly liberal", "Liberal", "Moderate", 
                                                      "Conservative", "Slightly conservative", "Extremely conservative"))

GSS2002 %>% 
  group_by(Religion, Politics) %>%
  filter(Religion == "Catholic" | Religion == "None" | Religion == "Protestant" | Religion == "Moslem/Islam") %>% 
  summarize(n()) %>% 
  na.omit() %>% 
  ggplot(aes(x = Religion, y = `n()`, fill = Politics)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(type = "div") + 
  theme_bw()
  