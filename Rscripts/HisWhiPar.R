# Alan Arnholt
# 9/26/17

library(readxl)
DF <- read_excel("TMP.xlsx")
head(DF)
# Problems with Age_Cohort
table(DF$Age_Cohort)
DF$Age_Cohort <- ifelse(DF$Age_Cohort == "42898", "6-12", DF$Age_Cohort)
table(DF$Age_Cohort)
levels(DF$Age_Cohort) <- c("0-5", "6-12", "13-17", "18-21", "22-50", "51 +")
table(DF$Age_Cohort)
library(dplyr)
library(ggplot2)
# Typical expenditure by Gender
DF %>% 
  group_by(Gender) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n())
DF %>% 
  group_by(Gender) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n()) %>%
  ggplot(aes(x = Gender, y= ME)) + 
  geom_bar(stat = "identity") 
# Tweaked
DF %>% 
  group_by(Gender) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n()) %>%
  ggplot(aes(x = Gender, y= ME, fill = Gender)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Average Expenditure by Gender", y = "Mean Expenditure") + 
  theme_bw() + 
  scale_fill_manual(values = c("purple", "blue"))
# Typical expenditure for Hispanics
DF %>% 
  group_by(Ethnicity) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n())
DF %>% 
  group_by(Ethnicity) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n()) %>%  
  ggplot(aes(x = reorder(Ethnicity, ME), y = ME)) +
  geom_bar(stat="identity")
# tweaked
DF %>% 
  group_by(Ethnicity) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n()) %>%  
  ggplot(aes(x = reorder(Ethnicity, ME), y = ME)) +
  geom_bar(stat="identity", fill = "red") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  labs(x = "", y = "Mean Expenditure", title = "Average Expenditure by Ethnicity")

# Typical expenditure for 22-50 yo
DF %>% 
  group_by(Age_Cohort) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n())
#
DF %>% 
  group_by(Age_Cohort) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n()) %>% 
  ggplot(aes(x = reorder(Age_Cohort, ME), y = ME)) + 
           geom_bar(stat = "identity")
# Tweaked
DF %>% 
  group_by(Age_Cohort) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n()) %>% 
  ggplot(aes(x = reorder(Age_Cohort, ME), y = ME)) + 
  geom_bar(stat = "identity", fill = "red") + 
  theme_bw() + 
  labs(x = "Age Cohort", y = "Average Expenditure", title = "Average Expenditure by Age Cohort")

# Male white-non-hispanic 
DF %>% 
  group_by(Ethnicity, Gender) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n())
#
DF %>% 
  group_by(Ethnicity, Gender) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n()) %>%
  ggplot(aes(x = reorder(Ethnicity, ME), y = ME, fill = Gender)) +
           geom_bar(stat = "identity", position = "dodge")
# tweaked
DF %>% 
  group_by(Ethnicity, Gender) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n()) %>%
  ggplot(aes(x = reorder(Ethnicity, ME), y = ME, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  labs(x = "", y = "Average Expenditure", title = "Average Expenditure by Ethnicity and Gender") + 
  scale_fill_manual(values = c("pink", "blue"))
  
  

# Asian 22-50 yo
DF %>% 
  group_by(Ethnicity, Age_Cohort) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n())
#
DF %>% 
  group_by(Ethnicity, Age_Cohort) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n()) %>% 
  ggplot(aes(x = reorder(Ethnicity, ME), y = ME, fill = Age_Cohort)) +
  geom_bar(stat = "identity", position = "dodge")
# Last graph is too crowded
DF %>% 
  group_by(Ethnicity, Age_Cohort) %>% 
  summarize(ME = mean(Expenditures), MDE = median(Expenditures), n= n()) %>% 
  ggplot(aes(x = reorder(Age_Cohort, ME), y = ME, fill = Age_Cohort)) +
  geom_bar(stat = "identity") + 
  facet_grid(.~Ethnicity) + 
  theme_bw()
#### Just consider White and Hispanic
DF %>%
  filter(Ethnicity %in% c("Hispanic", "White not Hispanic")) %>% 
  group_by(Ethnicity) %>% 
  summarize(ME = mean(Expenditures), n = n())
#### Consider Bivariate analysis of Age Cohort now
DF %>%
  filter(Ethnicity %in% c("Hispanic", "White not Hispanic")) %>% 
  group_by(Age_Cohort, Ethnicity) %>% 
  summarize(ME = mean(Expenditures), n = n())
# Graph now with tweaks
DF %>%
  filter(Ethnicity %in% c("Hispanic", "White not Hispanic")) %>% 
  group_by(Age_Cohort, Ethnicity) %>% 
  summarize(ME = mean(Expenditures), n = n()) %>%
  ggplot(aes(x = reorder(Age_Cohort, ME), y = ME, fill = Ethnicity)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_bw() + 
  labs(x = "Age Cohort", y = "Average Expenditure", 
       title = "Average Expenditures by Age Cohort and Ethnicity") + 
  scale_fill_manual(values = c("chocolate", "peachpuff"))
## Some Explanation
DF %>%
  filter(Ethnicity %in% c("Hispanic", "White not Hispanic")) %>% 
  group_by(Age_Cohort, Ethnicity) %>% 
  summarize(ME = mean(Expenditures), n = n())
##
DF %>%
  filter(Ethnicity %in% c("Hispanic", "White not Hispanic")) %>% 
  group_by(Age_Cohort, Ethnicity) %>% 
  summarize(ME = mean(Expenditures), n = n()) %>%
  ggplot(aes(x = reorder(Age_Cohort, ME), y = n, fill = Ethnicity)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_bw() + 
  scale_fill_manual(values = c("chocolate", "peachpuff")) + 
  labs(x = "Age Cohort", y = "Number in Group", title = "Consumers by Ethnicity and Age Cohort")
