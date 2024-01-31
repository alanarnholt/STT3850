
# loading packages
library(nycflights13)
library(ggplot2)
library(dplyr)


flights %>% 
  filter(carrier == "AS") %>% 
  select(year, month, arr_delay, dep_delay) %>% 
  head(n = 3)


portland_flights <- flights %>% 
  filter(dest == "PDX")
# View(portland_flights)

btv_sea_flights_fall <- flights %>% 
  filter(origin == "JFK" & (dest == "BTV" | dest == "SEA") & 
         month >= 10)
# View(btv_sea_flights_fall)

btv_sea_flights_fall <- flights %>% 
  filter(origin == "JFK", (dest == "BTV" | dest == "SEA"), 
         month >= 10)
# View(btv_sea_flights_fall)

not_BTV_SEA <- flights %>% 
  filter(!(dest == "BTV" | dest == "SEA"))
# View(not_BTV_SEA)

flights %>% 
  filter(!dest == "BTV" | dest == "SEA")

many_airports <- flights %>% 
  filter(dest == "SEA" | dest == "SFO" | dest == "PDX" | 
         dest == "BTV" | dest == "BDL")

many_airports <- flights %>% 
  filter(dest %in% c("SEA", "SFO", "PDX", "BTV", "BDL"))
# View(many_airports)


summary_temp <- weather %>% 
  summarize(mean = mean(temp), std_dev = sd(temp))
summary_temp

summary_temp <- weather %>% 
  summarize(mean_temp = mean(temp, na.rm = TRUE), 
            sd_temp = sd(temp, na.rm = TRUE))
summary_temp


summary_monthly_temp <- weather %>% 
  group_by(month) %>% 
  summarize(mean = mean(temp, na.rm = TRUE), 
            std_dev = sd(temp, na.rm = TRUE),
            count = n())
summary_monthly_temp %>% head(n = 3)

by_origin_monthly <- flights %>% 
  group_by(origin, month) %>% 
  summarize(count = n())
by_origin_monthly[1:5, ]

by_origin_monthly_incorrect <- flights %>% 
  group_by(origin) %>% 
  group_by(month) %>% 
  summarize(count = n())
by_origin_monthly_incorrect


weather %>% 
  mutate(temp_in_C = (temp - 32) / 1.8,
         monthT = month.name[month]) -> weather
weather %>% 
  select(monthT, month)

summary_monthly_temp <- weather %>% 
  group_by(month) %>% 
  summarize(mean_temp_in_F = mean(temp, na.rm = TRUE), 
            mean_temp_in_C = mean(temp_in_C, na.rm = TRUE)) 
summary_monthly_temp %>% 
  head(n = 5)

flights <- flights %>% 
  mutate(gain = dep_delay - arr_delay)
flights  %>% 
  select(dep_delay, arr_delay, gain) -> flights_gain

flights_gain

gain_summary <- flights %>% 
  summarize(
    Min = min(gain, na.rm = TRUE),
    Q1 = quantile(gain, 0.25, na.rm = TRUE),
    Median = quantile(gain, 0.5, na.rm = TRUE),
    Q3 = quantile(gain, 0.75, na.rm = TRUE),
    Max = max(gain, na.rm = TRUE),
    Mean = mean(gain, na.rm = TRUE),
    SD = sd(gain, na.rm = TRUE),
    Missing = sum(is.na(gain))
  )
gain_summary

ggplot(data = flights, mapping = aes(x = gain)) +
  geom_histogram(color = "black", fill = "lightblue", 
                 bins = 20) + 
  theme_bw() + 
  labs(title = "Distribution of Gain")

freq_dest <- flights %>% 
  group_by(dest) %>% 
  summarize(num_flights = n())
freq_dest %>% head(n = 5)

freq_dest %>% 
  arrange(num_flights) %>% 
  head(n = 5)

freq_dest %>% 
  arrange(desc(num_flights)) %>% 
  head(n = 5)


flights_joined <- flights %>% 
  inner_join(airlines, by = "carrier")
# View(flights)
# View(flights_joined)


# View(airports)

flights_with_airport_names <- flights %>% 
  inner_join(airports, by = c("dest" = "faa"))
# View(flights_with_airport_names)

named_dests <- flights %>%
  group_by(dest) %>%
  summarize(num_flights = n()) %>%
  arrange(desc(num_flights)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  rename(airport_name = name)
named_dests[1:2, 1:3]

flights_weather_joined <- flights %>%
  inner_join(weather, by = c("year", "month", "day", 
                             "hour", "origin"))
flights_weather_joined[1:5, 1:6]
# View(flights_weather_joined)


#glimpse(flights)

flights_sub <-flights %>% 
  select(carrier, flight)

flights_no_year <- flights %>% select(-year)

names(flights)
flight_arr_times <- flights %>% 
  select(month:day, arr_time:sched_arr_time)
#flight_arr_times

flights_reorder <- flights %>% 
  select(year, month, day, hour, minute, time_hour, 
         everything())
# glimpse(flights_reorder)

flights_sub1 <- flights %>% select(starts_with("a"))
flights_sub2 <- flights %>% select(ends_with("delay"))
flights_sub3 <- flights %>% select(contains("time"))


library(fivethirtyeight)
drinks_smaller <- drinks %>% 
  filter(country %in% c("USA", "China", "Italy", 
                        "Saudi Arabia")) %>% 
  select(-total_litres_of_pure_alcohol) %>% 
  rename(beer = beer_servings, spirit = spirit_servings, 
         wine = wine_servings)
drinks_smaller


library(tidyr)
drinks_smaller_tidy <- drinks_smaller %>% 
  pivot_longer(names_to = "type", 
               values_to = "servings", 
               cols = -country)

drinks_smaller_tidy

library(tidyr)
drinks_smaller %>% 
  pivot_longer(names_to = "type", 
               values_to = "servings", 
               cols = c(beer, spirit, wine))
# Or
drinks_smaller %>% 
  pivot_longer(names_to = "type", 
               values_to = "servings", 
               cols = beer:wine)

library(tidyr)
ggplot(drinks_smaller_tidy, aes(x = country, y = servings, 
                                fill = type)) +
  geom_col(position = "dodge") + 
  theme_bw()

