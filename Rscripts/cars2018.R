# Cars 2018
library(tidyverse)
cars2018 <- read_csv("./Data/cars2018.csv")

# Print the cars2018 object
glimpse(cars2018)


cars2018 |> 
  ggplot(aes(x = mpg)) +
  geom_histogram(bins = 25)

cars2018 <- cars2018 |> 
  mutate(log10_mpg = log10(mpg)) |> 
  select(-mpg)

############ Regression with categorical variable
mod_cat <- lm(log10_mpg ~ drive, data = cars2018)
mod_cat
# Dummy Variables - note that the 2-wheel Drive, front is the reference level
model.matrix(mod_cat) -> mod_mat
mod_mat |> 
  bind_cols(cars2018 |> select(drive)) -> TS
TS
# Consider
lm(log10_mpg ~ drive + 0, data = cars2018)
#####
cars2018 |> 
  group_by(drive) |> 
  summarize(MeanLog10_MPG = mean(log10_mpg))
################################################

cars2018 |> 
  ggplot(aes(x = log10_mpg)) +
  geom_histogram(bins = 15)


ggplot(data = cars2018, aes(x = displacement, y = log10_mpg)) + 
  geom_point()

ggplot(data = cars2018, aes(x = displacement, y = log10_mpg, color = drive)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# OR

ggplot(data = cars2018, aes(x = displacement, y = log10_mpg, color = drive)) + 
  geom_point() + 
  moderndive::geom_parallel_slopes(se = FALSE)


###########
full_mod <- lm(log10_mpg ~ displacement + drive + displacement:drive, data = cars2018)
summary(full_mod)

red_mod <- lm(log10_mpg~ displacement + drive, data = cars2018)
summary(red_mod)
#
anova(red_mod, full_mod)

##########
ggplot(data = cars2018, aes(x = displacement, y = log10_mpg, color = drive)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() + 
  facet_wrap(vars(recommended_fuel, transmission))

cars_sub_premium_auto <- cars2018 |> 
  filter(transmission == "Automatic", 
         recommended_fuel == "Premium Unleaded Recommended")


ggplot(data = cars_sub_premium_auto, 
       aes(x = displacement, y = log10_mpg, color = drive)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() 
# Or
ggplot(data = cars_sub_premium_auto, 
       aes(x = displacement, y = log10_mpg, color = drive)) + 
  geom_point() + 
  moderndive::geom_parallel_slopes(se = FALSE) + 
  theme_bw() 
 
#####
###########
full_mod <- lm(log10_mpg ~ displacement + drive + displacement:drive, 
               data = cars_sub_premium_auto)
summary(full_mod)

red_mod <- lm(log10_mpg~ displacement + drive, 
              data = cars_sub_premium_auto)
summary(red_mod)
#
anova(red_mod, full_mod)

library(moderndive)
get_regression_table(red_mod) -> T3
T3
T3$estimate
T3$estimate[1] -> FW2
sum(T3$estimate[c(1, 3)]) -> RW2
sum(T3$estimate[c(1, 4)]) -> FourW
sum(T3$estimate[c(1, 5)]) -> AWD
c(FW2, RW2, FourW, AWD)
######

ggplot(data = cars_sub_premium_auto, 
       aes(x = displacement, y = log10_mpg, color = drive)) + 
  geom_point() + 
  moderndive::geom_parallel_slopes(se = FALSE) + 
  theme_bw() +
  geom_abline(intercept = FW2, slope = -0.019, color = "red", lty = "dashed") + 
  geom_abline(intercept = RW2, slope = -0.019, color = "green", lty = "dashed") + 
  geom_abline(intercept = FourW, slope = -0.019, color = "lightblue", lty = "dashed") + 
  geom_abline(intercept = AWD, slope = -0.019, color = "purple", lty = "dashed")
