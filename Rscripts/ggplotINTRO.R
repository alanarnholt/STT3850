library(ggplot2)
library(nycflights13)
summary(flights)
sub_flights <- flights[1000:1200, ]
ggplot(data = sub_flights, aes(x = arr_delay, y = dep_delay)) + 
  geom_point(color = "red") + 
  labs(x = "Arrival Delay", y = "Departure Delay") + 
  theme_bw() + 
  geom_smooth( se = FALSE)
?geom_smooth
