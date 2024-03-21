# Made up data
set.seed(123)
area <- seq(800, 5000, length= 100) + rnorm(100, 0, 100)
lot <- seq(.2, 5, length = 100) + abs(rnorm(100, 0, .2))
location <- rep(c("Great", "Good", "Bad"), length = 100)
price <- area*200 + lot*50000 + ifelse(location == "Great", 50000, ifelse(location =="Good", 0, -50000)) + rnorm(100, 0, 150000)
plot(lot, price)
plot(area, price)

mod <- lm(price ~ area + lot + location)
summary(mod)
HDF <- data.frame(price, lot, location, area)
rm(price, lot, location, area)
head(HDF)
write.csv(HDF, "./Data/HP.csv")


library(tidyverse)
ggplot(data = HDF, aes(x = area, y = price, color = location)) + 
  geom_point() +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE)
library(moderndive)
ggplot(data = HDF, aes(x = lot, y = price, color = location)) + 
  geom_point() +
  theme_bw() + 
  geom_parallel_slopes(se = FALSE)

library(plotly)
p <- plot_ly(data = HDF, z = ~price, x = ~area, y = ~lot) %>%
  add_markers(color = ~location)
p
summary(mod)$coef

x <- seq(min(HDF$area), max(HDF$area), length = 100)
y <- seq(min(HDF$lot), max(HDF$lot), length = 100)

planeBAD <- outer(x, y, function(a, b){summary(mod)$coef[1,1] + 
    summary(mod)$coef[2,1]*a + summary(mod)$coef[3,1]*b})

planeGOOD <- outer(x, y, function(a, b){summary(mod)$coef[1,1] + 
    summary(mod)$coef[2,1]*a + summary(mod)$coef[3,1]*b + summary(mod)$coef[4,1]})

planeGREAT <- outer(x, y, function(a, b){summary(mod)$coef[1,1] + 
    summary(mod)$coef[2,1]*a + summary(mod)$coef[3,1]*b + summary(mod)$coef[5,1]})


p %>%
  add_surface(x = ~x, y = ~y, z = ~planeBAD, showscale = FALSE) %>%
  add_surface(x = ~x, y = ~y, z = ~planeGOOD, showscale = FALSE) %>%
  add_surface(x = ~x, y = ~y, z = ~planeGREAT, showscale = FALSE)


