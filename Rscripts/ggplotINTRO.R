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

###################################################################################

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

############################################
library(tidyverse)
library(moderndive)
library(PASWR2)
str(VIT2005)
### Munging: making elevator a factor
VIT2005 <- VIT2005 %>% 
  mutate(elevator = as.factor(elevator))
str(VIT2005)
ggplot(data = VIT2005, aes(x = area, y = totalprice)) + 
  geom_point() + 
  theme_bw()

mod_simple <- lm(totalprice ~ area, data = VIT2005)
summary(mod_simple)
get_regression_table(mod_simple)
get_regression_points(mod_simple)
##################################################################################
##################################################################################
##################################################################################
##################################################################################
#### Look at the association
### Definition
# (1/(n-1))*sum(Zx * Zy)
n <- dim(VIT2005)[1]
VIT2005 %>% 
  mutate(Zx = (area - mean(area))/sd(area),
         Zy = (totalprice - mean(totalprice))/sd(totalprice)
  ) %>% 
  select(area, totalprice, Zx, Zy) -> IDF
IDF

IDF %>% 
  summarize(r = (1/(n - 1))*sum(Zx * Zy))
#############################################################
# Tidyverse
VIT2005 %>% 
  summarize(COR = cor(area, totalprice))
# Moderndive
VIT2005 %>% 
  moderndive::get_correlation(formula = totalprice ~ area)
# Base R
cor(VIT2005$totalprice, VIT2005$area)
#############################################################

ggplot(data = VIT2005, aes(x = area, y = totalprice, color = elevator)) +
  geom_point() +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE)

#############################################################

mod_int1 <- lm(totalprice ~ area + elevator + elevator:area, data = VIT2005)
summary(mod_int1)
get_regression_table(mod_int1)
#### Note: totalprice ~ area + elevator + elevator:area 
#### same as: totalprice ~ area*elevator
mod_int2 <- lm(totalprice ~ area*elevator, data = VIT2005)
summary(mod_int2)
####
#### Line for Elevators: totalprice_hat = 113114.0 -50871.7 + (1343.7 + 1202.0)*area
#### Line for no elevators: totalprice_hat = 113114 + 1347.7*area
####
ggplot(data = VIT2005, aes(x = area, y = totalprice, color = elevator)) + 
  geom_point() + 
  theme_bw() +
  geom_abline(intercept = 113114, slope = 1347.7, color = "red") +
  geom_abline(intercept = 113114-50871.7, slope = 1343.7 + 1202.0, color = "blue") + 
  scale_color_manual(values = c("red", "blue"))
####
summary(mod_int1)
####

#### To determine if the lines are the same (which means that the linear relationship 
#### between appraised price and living area is the same for apartments with and without 
#### elevators), the hypotheses are H_0: beta_2 = beta_3 = 0 vs H_A: at least one beta_i 
#### ne 0 for i = 2, 3.

mod_simple <- lm(totalprice ~ area, data = VIT2005)
anova(mod_simple, mod_int1)

### In this problem, one may conclude that at least one of beta_2 and beta_3 is not
### zero since the p-value = 0.  In other words, the lines have either different 
### intercepts, different slopes, or different intercepts and slopes.

#### To test if the lines have the same slopes (which means that the presence of an 
#### elevator adds constant value over all possible living area)the hypotheses are
#### H_0: beta_3 = 0 vs. H_A: beta_3 ne 0.

summary(mod_int1)

### Base on a p-value of 0.004380t may be concluded that beta_3 ne 0, which implies 
### that the lines are not parallel.

#### To test for equal intercepts (which means that appraised price with and without 
#### elevators starts at the same value), the hypotheses are
#### H_0: beta_2 = 0 vs H_A: beta_2 ne 0.

mod_ib <- lm(totalprice ~ area + area:elevator, data = VIT2005)
anova(mod_ib, mod_int1)

## Since the p-value = 0.1133 one should conclude that the two lines have the same 
## intercept but different slopes.

summary(mod_ib)

### Correct lines----

ggplot(data = VIT2005, aes(x = area, y = totalprice, color = elevator)) +
  geom_point() + 
  geom_abline(intercept = 71352.08, slope = 1897.94 + 553.99, color = "blue") + 
  geom_abline(intercept = 71352.08, slope = 1897.94, color = "red") + 
  theme_bw() + 
  scale_color_manual(values = c("red", "blue")) + 
  xlim(0, 200) + 
  ylim(0, 600000)

#####

### Compare:
summary(mod_ib) # adjusted R2: 0.7034
### Versus:
summary(mod_simple) # adjusted R2: 0.65532

######################
## If we wanted parallel lines the easiest way to get them is to use
## geom_parallel_slopes() from moderndive

ggplot(data = VIT2005, aes(x = area, y = totalprice, color = elevator)) + 
  geom_point() + 
  geom_parallel_slopes() + 
  theme_bw()
