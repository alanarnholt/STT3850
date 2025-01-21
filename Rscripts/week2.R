# Week 2
# Different types of graphs
# Five Named Graphs
# 01/20/2025

library(tidyverse)
mtcars
mtcars <- mtcars |> 
  mutate(cyl = factor(cyl),
         vs = factor(vs, label = c("V-shaped", "straight")),
         am = factor(am, label = c("automatic", "manual")),
         gear = factor(gear),
         carb = factor(carb))
str(mtcars)

# 5NG
# scatterplot
ggplot(data = mtcars, aes(x = mpg, y = wt)) +
  geom_point()
#
ggplot(data = mtcars, aes(x = mpg, y = wt)) +
  geom_point() + 
  facet_grid(rows = vars(vs), cols = vars(am))
#
ggplot(data = mtcars, aes(x = mpg, y = wt)) +
  geom_point() + 
  facet_grid(rows = vars(vs), cols = vars(am)) + 
  geom_smooth(method = "lm", se = FALSE)
#
ggplot(data = mtcars, aes(x = mpg, y = wt)) +
  geom_point() + 
  facet_grid(rows = vars(vs), cols = vars(am)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() + 
  labs(x = "Miles per Gallon", 
       y = "Weight of the vehicle in 1000 lbs",
       title = "Some informative title goes here")

# 5NG#2: Line Graphs - 
library(ggthemes)
library(nycflights13)
ggplot(data = weather, aes(x = time_hour, y = temp)) + 
  geom_line()
#
ggplot(data = weather, aes(x = time_hour, y = temp)) + 
  geom_line() + 
  theme_wsj()

# 5NG#3: Histograms
ggplot(data = mtcars, aes(x = disp)) +
  geom_histogram()
# 
ggplot(data = mtcars, aes(x = disp)) +
  geom_histogram(binwidth = 50)
# 
ggplot(data = mtcars, aes(x = disp)) +
  geom_histogram(binwidth = 50) +
  facet_wrap(vars(cyl))
# 
ggplot(data = mtcars, aes(x = disp)) +
  geom_histogram(binwidth = 40, fill = "blue", color = "black") +
  facet_wrap(vars(cyl), ncol = 1) + 
  theme_bw() + 
  labs(x = "Displacement in cubic inches")
# 
# 5NG#4: Boxplots

ggplot(data = mtcars, aes(x = am, y = wt)) + 
  geom_boxplot()

# 5NG#5: Barplots

mtcars |> 
  count(vs)

mtcars |> 
  count(vs) |> 
  ggplot(aes(x = vs, y = n)) + 
  geom_col()
# Or
mtcars |> 
  ggplot(aes(x = vs)) + 
  geom_bar()
#
mtcars |> 
  ggplot(aes(x = vs)) + 
  geom_bar(fill = "lightblue", color = "black") + 
  theme_classic()
library(ggthemes)
mtcars |> 
  ggplot(aes(x = vs)) + 
  geom_bar(fill = "lightblue", color = "black") + 
  theme_wsj()
