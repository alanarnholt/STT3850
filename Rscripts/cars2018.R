# Cars 2018
library(tidyverse)
cars2018 <- read_csv("./Data/cars2018.csv")

# Print the cars2018 object
glimpse(cars2018)


cars2018 |> 
  ggplot(aes(x = mpg)) +
  geom_histogram(bins = 25)

cars2018 <- cars2018 |> 
  mutate(mpg = log10(mpg))

cars2018 |> 
  ggplot(aes(x = mpg)) +
  geom_histogram(bins = 15)


ggplot(data = cars2018, aes(x = displacement, y = mpg)) + 
  geom_point()

ggplot(data = cars2018, aes(x = displacement, y = mpg, color = drive)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# OR

ggplot(data = cars2018, aes(x = displacement, y = mpg, color = drive)) + 
  geom_point() + 
  moderndive::geom_parallel_slopes(se = FALSE)


###########
full_mod <- lm(mpg ~ displacement + drive + displacement:drive, data = cars2018)
summary(full_mod)

red_mod <- lm(mpg~ displacement + drive, data = cars2018)
summary(red_mod)
#
anova(red_mod, full_mod)

##########
ggplot(data = cars2018, aes(x = displacement, y = mpg, color = drive)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() + 
  facet_wrap(vars(recommended_fuel, transmission))

cars_sub_premium_auto <- cars2018 |> 
  filter(transmission == "Automatic", 
         recommended_fuel == "Premium Unleaded Recommended")


ggplot(data = cars_sub_premium_auto, 
       aes(x = displacement, y = mpg, color = drive)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() 
# Or
ggplot(data = cars_sub_premium_auto, 
       aes(x = displacement, y = mpg, color = drive)) + 
  geom_point() + 
  moderndive::geom_parallel_slopes(se = FALSE) + 
  theme_bw() 
 
#####
###########
full_mod <- lm(mpg ~ displacement + drive + displacement:drive, 
               data = cars_sub_premium_auto)
summary(full_mod)

red_mod <- lm(mpg~ displacement + drive, 
              data = cars_sub_premium_auto)
summary(red_mod)
#
anova(red_mod, full_mod)
