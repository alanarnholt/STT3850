library(tidyverse)
library(moderndive)
str(evals)

xtabs(~ethnicity + language, data = evals) -> T1
T1
addmargins(T1)
chisq.test(T1)
chisq.test(T1)$exp

xtabs(~ethnicity + rank, data = evals) -> T2
T2
chisq.test(T2)
chisq.test(T2)$exp

xtabs(~gender + rank, data = evals) -> T3
T3
chisq.test(T3)
chisq.test(T3)$exp
obs_stat <- chisq.test(T3)$stat
obs_stat

B <- 10^4
x2v <- numeric(B)
for(i in 1:B){
  x2v[i] <- chisq.test(xtabs(~gender + sample(rank), data = evals))$stat
}
hist(x2v, freq = FALSE, col = "blue", ylim = c(0, 0.5))
curve(dchisq(x, 2), add = TRUE, col = "red")

(pvalue <- (sum(x2v >= obs_stat) + 1)/(B + 1))

library(infer)
evals %>% 
  specify(gender ~ rank) %>% 
  hypothesize(null = "independence") %>% 
  calculate(stat = "Chisq") %>% 
  pull() -> obs_stat_infer
obs_stat_infer

null_dist <- evals %>% 
  specify(gender ~ rank) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 10^3, type = "permute") %>% 
  calculate(stat = "Chisq")
head(null_dist)

null_dist %>% 
  visualize() + 
  shade_p_value(obs_stat_infer, direction = "greater")



#############################################################

library(resampledata)
names(GSS2002)
T4 <- xtabs(~Marital + Happy, data = GSS2002)
T4
chisq.test(T4)


T5 <- xtabs(~Education + Gender, data = GSS2002)
T5
chisq.test(T5)









###########################################################
limitRange <- function(fun, df, min, max){
  function(x){
    y <- fun(x, df)
    y[x < min | x > max] <- NA
    return(y)
  }
}


limitRangeC <- function(fun, df, min, max){
  function(x){
    y <- fun(x, df)
    y[x < max & x > min] <- NA
    return(y)
  }
}


dlimit <- limitRange(fun = dchisq, df = 5,
                     min = qchisq(.10/2, 5), max = qchisq(1-.10/2, 5))

dlimitC <- limitRangeC(fun = dchisq, df = 5,
                      min = qchisq(.10/2, 5), max = qchisq(1-.10/2, 5))

dlimitC(0:(4*5))
p <- ggplot(data = data.frame(x = 0:(4*5)), aes(x = x))
p + stat_function(fun = dchisq, args = list(df = 5), n = 200) + 
    stat_function(fun = dlimit, geom = "area", fill = "blue", 
                  alpha = .3, n = 1000) +
    stat_function(fun = dlimitC, geom = "area", fill = "green", 
                alpha = .9, n = 1000) + 
  theme_bw() + 
  labs(x = "", y = "") + 
  stat_function(fun = dchisq, args = list(df = 5), n = 1500) 
  
