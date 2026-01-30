# Week 3
# Alan T. Arnholt

library(nycflights13) 
library(ggplot2) 
library(dplyr)


flights |> 
  filter(carrier == "AS") |> 
  select(year, month, arr_delay, dep_delay)

flights |> 
  filter(dest == "PDX") -> portland_flights

flights |> 
  filter(origin =="JFK" & (dest == "BTV" | dest == "SEA") &
           month >=10) -> btv_sea_flights_fall 

flights |> 
  filter(!(dest =="BTV" | dest == "SEA")) -> not_BTV_SEA
not_BTV_SEA
View(not_BTV_SEA)

many_airports <- flights |> 
  filter(dest %in% c("SEA", "SFO", "PDX", "BTV", "BDL"))

View(many_airports)

weather |> 
  summarize(MEAN = mean(temp, na.rm = TRUE), 
            SD = sd(temp, na.rm = TRUE))

weather |> 
  group_by(month) |> 
  summarize(MEAN = mean(temp, na.rm = TRUE),
            SD = sd(temp, na.rm = TRUE),
            count = n())

flights |> 
  group_by(origin, month) |> 
  summarize(count = n())

flights |> 
  group_by(origin) |>
  group_by(month) |> 
  summarize(count = n())

weather |> 
  mutate(monthT = month.name[month]) |> 
  mutate(temp_in_C = (temp - 32)/1.8) |> 
  select(monthT, month, temp, temp_in_C)

weather |> 
  mutate(monthT = month.name[month]) |> 
  mutate(temp_in_C = (temp - 32)/1.8) |> 
  select(monthT, month, temp, temp_in_C) |> 
  group_by(monthT) |> 
  summarize(MD = median(temp, na.rm = TRUE)) |> 
  arrange(desc(MD))
