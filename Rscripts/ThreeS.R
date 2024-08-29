library(e1071)
library(tidyverse)
library(moderndive)
view(evals)

ggplot(data = evals, aes(x = age)) + 
  geom_density(fill = "red")

evals |> 
  summarize(M = mean(age), 
            MD = median(age),
            SD = sd(age),
            iqr = IQR(age),
            skew = skewness(age),
            n = n())
set.seed(123)
pineapple <- rexp(100, 0.1)
DF <- data.frame(x = pineapple)
DF

ggplot(data = DF, aes(x = x)) + 
  geom_density(fill = "pink")

median(DF$x)
IQR(DF$x)
DF |> 
  summarize(median(x), IQR(x))





ggplot(data = evals, aes(x = age)) + 
  geom_density()

evals |> 
  summarize(Mean = mean(age), Median = median (age),
            SD = sd(age), iqr = IQR(age), 
            skew = skewness(age), n = n())

set.seed(123)
junk <- rexp(100, .1)
DF <- data.frame(junk = junk)
rm(junk)

ggplot(data = DF, aes(x = junk)) + 
  geom_density(fill = "darkblue")
DF |> 
  summarize(Median = median(junk),
            iqr = IQR(junk),
            skew = skewness(junk))
evals |> 
  filter( age >= 50) |> 
  summarize(M = mean(age))
