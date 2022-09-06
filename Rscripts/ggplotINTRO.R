library(tidyverse)
library(nycflights13)
summary(flights)
set.seed(123)
ind <- sample(dim(flights)[1], 5000, 
              replace = FALSE)
sub_flights <- flights[ind, ]


ggplot(data = sub_flights, aes(x = dep_delay, 
                               y = arr_delay)) + 
  geom_point(color = "red", alpha = 0.1) + 
  labs(x = "Arrival Delay", 
       y = "Departure Delay", 
       title = "Some big long centered title goes here") + 
  theme_bw() + 
  geom_smooth(se = FALSE) + 
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = sub_flights, 
       aes(x = dep_delay, y = arr_delay)) + 
  geom_point() + 
  labs(y = "Arrival Delay", 
       x = "Departure Delay") + 
  theme_bw() + 
  geom_smooth(se = FALSE) + 
  facet_wrap(vars(carrier))

#### Departure Delays

ggplot(data = sub_flights, 
       aes(x = dep_delay)) + 
  geom_histogram()

ggplot(data = sub_flights, 
       aes(x = dep_delay)) + 
  geom_histogram(binwidth = 10, fill = "pink", color = "black") + 
  theme_bw() + 
  labs(x = "Departure Delay in minutes", 
       title = "Some big long centered title goes here") + 
  theme(plot.title = element_text(hjust = 0.5))


sub_flights %>% 
  summarize(Median = median(dep_delay, na.rm = TRUE), 
            iqr = IQR(dep_delay, na.rm = TRUE),
            Mean = mean(dep_delay, na.rm = TRUE),
            SD = sd(dep_delay, na.rm = TRUE))

ggplot(data = sub_flights, 
       aes(x = dep_delay)) + 
  geom_histogram(binwidth = 10, fill = "pink", color = "black") + 
  theme_bw() + 
  facet_wrap(vars(carrier))
  labs(x = "Departure Delay in minutes")
  
# Density Plot
ggplot(data = sub_flights, 
       aes(x = dep_delay)) + 
  geom_density(fill = "pink", color = "black") + 
  theme_bw() + 
  labs(x = "Departure Delay in minutes")  


ggplot(data = sub_flights, 
       aes(x = dep_delay)) + 
  geom_density(fill = "pink", color = "black") + 
  theme_bw() + 
  facet_wrap(vars(carrier))
  labs(x = "Departure Delay in minutes")  
  
# Boxplots
ggplot(data = sub_flights, 
       aes(x = carrier, y = dep_delay)) + 
    geom_boxplot() + 
    theme_bw() + 
    labs(y = "Departure Delay in minutes", 
         x= "Airline Carrier") 


sub_flights %>% 
  filter(origin == "JFK" & (dest == "BTV" | dest == "SEA") & month >= 10) -> fall_flights
fall_flights

#### Using summarize

weather %>% 
  summarize(Mean = mean(temp, na.rm = TRUE),
            SD = sd(temp, na.rm = TRUE))

# By Month

weather %>% 
  group_by(month) %>% 
  summarize(Mean = mean(temp, na.rm = TRUE),
            SD = sd(temp, na.rm = TRUE)) -> summary_weather
summary_weather

summary_weather[summary_weather$month==6, ]
# Or
summary_weather %>% 
  filter(month == 6)

# Make a new variable --- mutate()

weather %>% 
  select(humid, pressure, temp) %>% 
  mutate(temp_in_C = (temp - 32)/1.8) -> newdf
newdf  

#
weather %>% 
  filter(origin == "JFK" & month == 6) %>%  
  ggplot(aes(x = temp)) +
  geom_histogram(binwidth = 10, color = "black", fill = "lightblue")


weather %>% 
  filter(origin == "JFK" & month == 6) %>% 
  select(temp) %>% 
  summarize(Skew = e1071::skewness(temp),
            Mean = mean(temp),
            Median = median(temp),
            SD = sd(temp),
            iqr = IQR(temp))

