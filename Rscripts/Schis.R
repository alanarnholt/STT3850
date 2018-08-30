# Schistomiasis

group <- c(rep("Treatment", 5), rep("Control", 5))
worms <- c(1, 2, 2, 10, 7, 16, 10, 10, 7, 17)
schis <- data.frame(group, worms)
rm(group, worms)
schis
library(ggplot2)
ggplot(data = schis, aes(x = group, y = worms)) +
  geom_point() + 
  theme_bw()
# Add some jitter
ggplot(data = schis, aes(x = group, y = worms)) +
  geom_point(position = "jitter") + 
  theme_bw()
# Add some color to groups
ggplot(data = schis, aes(x = group, y = worms, color = group)) +
  geom_point(position = "jitter") + 
  theme_bw()
# Labels
ggplot(data = schis, aes(x = group, y = worms, color = group)) +
  geom_point(position = "jitter") + 
  theme_bw() + 
  labs(x = "", y = "Number of schistosomes", title = "K11777 results",
       subtitle = "Some reference goes here")
# OCD like me....how to ceter title?
ggplot(data = schis, aes(x = group, y = worms, color = group)) +
  geom_point(position = "jitter") + 
  theme_bw() + 
  labs(x = "", y = "Number of schistosomes", title = "K11777 results",
       subtitle = "Some reference goes here") + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 1.0))
##
library(dplyr)
ANS <- schis %>% 
  group_by(group) %>% 
  summarize(Mean = mean(worms), SD = sd(worms))
ANS
MD <- ANS[1, 2] - ANS[2, 2]
MD
MD$Mean
Worms <- schis$worms
Worms
#### One way
index <- sample(10, size = 5, replace = FALSE)
index
-index
# Note how this works
Worms[index]
Worms[-index]
#
Sims <- 10^4 - 1
result <- numeric(Sims)
set.seed(3)
for(i in 1:Sims){
  # sample of size 5, from 1 to 10, without replacement
  index <- sample(10, size = 5, replace = FALSE)
  result[i] <- mean(Worms[index]) - mean(Worms[-index])
}
pvalue <- (sum(result >= MD$Mean) + 1)/(Sims + 1)
pvalue
######
# 8/30/2018
# Class more ggplot2
# Scatterplot
library(dplyr)
library(ggplot2)
library(nycflights13)
all_alaska_flights <- flights %>% 
  filter(carrier == "AS")
all_alaska_flights
# Scatterplot
ggplot(data = all_alaska_flights, aes(x = dep_delay, y = arr_delay)) + 
  geom_point()
# Too many overlapping points!----fix with alpha
ggplot(data = all_alaska_flights, aes(x = dep_delay, y = arr_delay)) + 
  geom_point(alpha = 0.15) + 
  theme_bw()
# Linegraphs
View(weather)
early_january_weather <-  weather %>%
  filter(origin == "EWR", month == 1, day <= 15)
# Note using commas same as &
early_january_weather <-  weather %>%
  filter(origin == "EWR" & month == 1 & day <= 15)
# Hourly temp in Newark
ggplot(data = early_january_weather, aes(x = time_hour, y = temp)) + 
  geom_line(color = "skyblue", size = 1.5) + 
  labs(title = "Hourly temperature in Newark for January 1-15, 2013",
       x = "", y = "Temperture in Fahrenheit") + 
  theme_bw()
## Histograms
ggplot(data = weather, aes(x = temp)) + 
  geom_histogram()
## The default is UGLY
ggplot(data = weather, aes(x = temp)) + 
  geom_histogram(color = "black", fill = "purple", bins = 30) + 
  theme_bw()
## Facets
ggplot(data = weather, aes(x = temp)) + 
  geom_histogram(binwidth = 4, color = "black", fill = "skyblue") + 
  facet_wrap(~ month, nrow = 4)
##---not quite the solution---need to arrange months not alphabetically
ggplot(data = weather, aes(x = temp)) + 
  geom_histogram(binwidth = 4, color = "black", fill = "skyblue") + 
  facet_wrap(~ month.name[month], nrow = 4) + 
  theme_bw()
##
weather <- weather %>% 
  mutate(MONTH = factor(month.name[month], ordered = TRUE,
                        levels = c("January", "February", "March",
                                   "April", "May", "June", 
                                   "July", "August", "September",
                                   "October", "November", "December")) )
####
ggplot(data = weather, aes(x = temp)) + 
  geom_histogram(binwidth = 4, color = "black", fill = "skyblue") + 
  facet_wrap(~ MONTH, nrow = 4) + 
  theme_bw()
##
#### Density plots
ggplot(data = weather, aes(x = temp)) + 
  geom_density(color = "black", fill = "skyblue") + 
  facet_wrap(~ MONTH, nrow = 4) + 
  theme_bw()
#### Boxplots
ggplot(data = weather, aes(x = MONTH, y = temp)) + 
  geom_boxplot()
# Labels overlap!---fix
ggplot(data = weather, aes(x = MONTH, y = temp)) + 
  geom_boxplot(fill = "skyblue") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))  
###
# inner_join----join flight with airports---airport code in origin in flights and 
# airport code is in faa for airports
flights_namedports <- flights %>% 
  inner_join(airports, by = c("origin" = "faa"))
### Barprahs
ggplot(data = flights_namedports, aes(x = carrier, fill = name)) +
  geom_bar(position = "fill") + 
  theme_bw() + 
  labs(y = "Fraction")


