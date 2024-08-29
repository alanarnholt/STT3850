# Boxplots
#
library(tidyverse)
library(nycflights13)

# Problem

ggplot(data = weather, aes(x = month, y = temp)) + 
  geom_boxplot()

# Not what we want

ggplot(data = weather, aes(x = as.factor(month), y = temp)) + 
  geom_boxplot()

# Some better

weather |> mutate(monthT = factor(month, labels = month.abb)) -> weather

ggplot(data = weather, aes(x = monthT, y = temp)) + 
  geom_boxplot()

# 

ggplot(data = weather, aes(x = monthT, y = temp, fill = monthT)) + 
  geom_boxplot() + coord_flip() + theme_classic() + 
  guides(fill = "none") + 
  labs(x = "", y = "Temperature in Fahrenheit",
       title = "Hourly meterological data for LGA, JFK and EWR in 2013")
