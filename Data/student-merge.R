d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students


# Alan's additions
library(tidyverse)
library(janitor)
d3 <- d3 %>% 
  clean_names()
glimpse(d3)
# g3_x - mathematics
# g3_y - portugeses

d3 %>% 
  group_by(sex) %>% 
  summarize(Mean = mean(g3_x), SD = sd(g3_x), n = n())
#
t.test(d3$g3_x ~ d3$sex)
#
fm_grades <- d3 %>% 
  filter(sex == "F") %>% 
  select(g3_x) %>% 
  pull()
mm_grades <- d3 %>% 
  filter(sex == "M") %>% 
  select(g3_x) %>% 
  pull()
summary(fm_grades)
summary(mm_grades)

#####
# Bootstrap T

B <- 10000
BST <- numeric(B)
for(i in 1:B){
  bss1 <- sample(fm_grades, 198, replace = TRUE)
  bss2 <- sample(mm_grades, 184, replace = TRUE)
  BST[i] <- ((mean(bss1) - mean(bss2)) - (mean(fm_grades) - mean(mm_grades))) / sqrt(var(bss1)/198 + var(bss2)/184)
}
Q <- quantile(BST, probs = c(0.025, 0.975))
Q

CI <- c( (mean(fm_grades) - mean(mm_grades)) -Q[2]*sqrt(var(fm_grades)/198 + var(mm_grades)/184),
         (mean(fm_grades) - mean(mm_grades)) -Q[1]*sqrt(var(fm_grades)/198 + var(mm_grades)/184) )
CI
