## -------------------------------------------------------------------------------------------------
library(tidyverse)
library(infer)
set.seed(123)
pop <- rbinom(30, 1, .6)
mean(pop)
DF_pop <- data.frame(vote = ifelse(pop == 0, "no", "yes"))
head(DF_pop)
all_polls <- DF_pop %>% 
  specify(response = vote, success = "yes") %>% 
  hypothesize(null = "point", p = 0.6) %>% 
  generate(reps = 1000, type = "simulate") %>% 
  mutate(poll = replicate)
all_polls


## -------------------------------------------------------------------------------------------------
# Compute p-hat for each poll
ex1_props <- all_polls %>% 
  # Group by poll/replicate
  group_by(poll) %>% 
  # Calculate proportion of yes votes
  summarize(stat = mean(vote == "yes"))
# Review the result
ex1_props


## -------------------------------------------------------------------------------------------------
# Select one poll from which to resample
one_poll <- all_polls %>%
  # Filter for the first poll
  filter(poll == 1) %>%
  # Select vote
  select(vote)


## -------------------------------------------------------------------------------------------------
# Compute p-hat* for each resampled poll
ex2_props <- one_poll %>%
  # Specify vote as the response, where yes means success
  specify(response = vote, success = "yes") %>%
  # Generate 1000 reps of type bootstrap
  generate(reps = 1000, type = "bootstrap") %>% 
  # Calculate the summary stat "prop"
  calculate(stat = "prop")


## -------------------------------------------------------------------------------------------------
# Calculate variability of p-hat
ex1_props %>% 
  summarize(variability = sd(stat))


## -------------------------------------------------------------------------------------------------
# Calculate variability of p-hat*
ex2_props %>% 
  summarize(variability = sd(stat))


## -------------------------------------------------------------------------------------------------
# Combine data from both experiments
both_ex_props <- bind_rows(ex1_props, ex2_props, .id = "experiment")


## -------------------------------------------------------------------------------------------------
# Using both_ex_props, plot stat colored by experiment
ggplot(both_ex_props, aes(x = stat, color = experiment)) + 
  # Add a density layer with bandwidth 0.1
  geom_density(bw = 0.1) + 
  theme_bw()


## -------------------------------------------------------------------------------------------------
# Proportion of yes votes by poll
props <- all_polls %>% 
  group_by(poll) %>% 
  summarize(prop_yes = mean(vote == "yes"))
props


## -------------------------------------------------------------------------------------------------
# The true population proportion of yes votes
true_prop_yes <- 0.6

# Proportion of polls within 2SE
props %>%
  # Add column: is prop_yes in 2SE of 0.6
  mutate(is_in_conf_int = abs(prop_yes - true_prop_yes) < 2 * sd(prop_yes)) %>%
  # Calculate  proportion in conf int
  summarize(prop_in_conf_int = mean(is_in_conf_int))


## -------------------------------------------------------------------------------------------------
one_poll <- all_polls %>%
  filter(poll == 1) %>%
  select(vote)
one_poll_boot <- one_poll %>%
  specify(response = vote, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "prop")


## -------------------------------------------------------------------------------------------------
p_hat <- one_poll %>%
  # Calculate proportion of yes votes
  summarize(mean(vote == "yes")) %>%
  pull()


## -------------------------------------------------------------------------------------------------
# Create an interval of plausible values
one_poll_boot %>%
  summarize(
    # Lower bound is p_hat minus 2 std errs
    lower = p_hat - 2 * sd(stat),
    # Upper bound is p_hat plus 2 std errs
    upper = p_hat + 2 * sd(stat)
  )


## -------------------------------------------------------------------------------------------------
p_hat <- one_poll %>%
  # Calculate proportion of yes votes
  summarize(mean(vote == "yes")) %>%
  pull()
p_hat


## -------------------------------------------------------------------------------------------------
# Create an interval of plausible values
one_poll_boot %>%
  summarize(
    # Lower bound is p_hat minus 2 std errs
    lower = p_hat - 2 * sd(stat),
    # Upper bound is p_hat plus 2 std errs
    upper = p_hat + 2 * sd(stat)
  )


## -------------------------------------------------------------------------------------------------
# From previous exercise: bootstrap t-confidence interval
one_poll_boot %>%
  summarize(
    lower = p_hat - 2 * sd(stat),
    upper = p_hat + 2 * sd(stat)
  )


## -------------------------------------------------------------------------------------------------
# Manually calculate a 95% percentile interval
one_poll_boot %>%
  summarize(
    lower = quantile(stat, p = 0.025),
    upper = quantile(stat, p = 0.975)
  )


## -------------------------------------------------------------------------------------------------
# Calculate the same interval, more conveniently
percentile_ci <- one_poll_boot %>% 
  get_confidence_interval(level = 0.95)
# Review the value
percentile_ci


## -------------------------------------------------------------------------------------------------
one_poll_boot %>% 
  # Visualize in-between the endpoints given by percentile_ci
  visualize(fill = "purple") + 
  shade_confidence_interval(endpoints = percentile_ci) +
  theme_bw()


## -------------------------------------------------------------------------------------------------
# Creating one_poll_boot_300
set.seed(123)
pop300 <- rbinom(300, 1, .6)
mean(pop300)
DF_pop300 <- data.frame(vote = ifelse(pop300 == 0, "no", "yes"))
# head(DF_pop300)
all_polls300 <- DF_pop300 %>% 
  specify(response = vote, success = "yes") %>% 
  hypothesize(null = "point", p = 0.6) %>% 
  generate(reps = 1000, type = "simulate") %>% 
  mutate(poll = replicate)
one_poll300 <- all_polls300 %>%
  filter(poll == 1) %>%
  select(vote)
one_poll_boot_300 <- one_poll300 %>%
  specify(response = vote, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "prop")
# Creating one_poll_boot_3
set.seed(123)
pop3 <- rbinom(3, 1, .6)
mean(pop3)
DF_pop3 <- data.frame(vote = ifelse(pop3 == 0, "no", "yes"))
# head(DF_pop3)
all_polls3 <- DF_pop3 %>% 
  specify(response = vote, success = "yes") %>% 
  hypothesize(null = "point", p = 0.6) %>% 
  generate(reps = 1000, type = "simulate") %>% 
  mutate(poll = replicate)
one_poll3 <- all_polls3 %>%
  filter(poll == 1) %>%
  select(vote)
one_poll_boot_3 <- one_poll3 %>%
  specify(response = vote, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "prop")
# Creating one_poll_boot_0.8
set.seed(123)
pop30 <- rbinom(30, 1, .8)
mean(pop30)
DF_pop30 <- data.frame(vote = ifelse(pop30 == 0, "no", "yes"))
# head(DF_pop3)
all_polls30 <- DF_pop30 %>% 
  specify(response = vote, success = "yes") %>% 
  hypothesize(null = "point", p = 0.8) %>% 
  generate(reps = 1000, type = "simulate") %>% 
  mutate(poll = replicate)
one_poll_0.8 <- all_polls30 %>%
  filter(poll == 1) %>%
  select(vote)
one_poll_boot_0.8 <- one_poll_0.8 %>%
  specify(response = vote, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "prop")


## -------------------------------------------------------------------------------------------------
calc_t_conf_int <- function(resampled_dataset) {
  resampled_dataset %>%
    summarize(
      lower = p_hat - 2 * sd(stat),
      upper = p_hat + 2 * sd(stat)
    )
}


## -------------------------------------------------------------------------------------------------
# Find the bootstrap t-confidence interval for n = 30
calc_t_conf_int(one_poll_boot)



## -------------------------------------------------------------------------------------------------
# Find the bootstrap t-confidence interval for n = 300
calc_t_conf_int(one_poll_boot_300)


## -------------------------------------------------------------------------------------------------
# Find the bootstrap t-confidence interval for n = 3 
calc_t_conf_int(one_poll_boot_3)


## -------------------------------------------------------------------------------------------------
calc_p_hat <- function(dataset) {
  dataset %>%
    summarize(stat = mean(vote == "yes")) %>%
    pull()
}
calc_t_conf_int <- function(resampled_dataset, p_hat) {
  resampled_dataset %>%
    summarize(
      lower = p_hat - 2 * sd(stat),
      upper = p_hat + 2 * sd(stat)
    )
}


## -------------------------------------------------------------------------------------------------
# Find proportion of yes votes from original population
p_hat <- calc_p_hat(one_poll)
# Review the value
p_hat  
# Calculate bootstrap t-confidence interval (original 0.6 param)
calc_t_conf_int(one_poll_boot, p_hat)


## -------------------------------------------------------------------------------------------------
# Find proportion of yes votes from new population
p_hat_0.8 <- calc_p_hat(one_poll_0.8)
  
# Review the value
p_hat_0.8  


## -------------------------------------------------------------------------------------------------
# Calculate the bootstrap t-confidence interval (new 0.8 param)
calc_t_conf_int(one_poll_boot_0.8, p_hat_0.8)


## -------------------------------------------------------------------------------------------------
# Calculate a 95% bootstrap percentile interval
one_poll_boot %>% 
  get_confidence_interval(level = 0.95) 


## -------------------------------------------------------------------------------------------------
# Calculate a 99% bootstrap percentile interval
one_poll_boot %>% 
  get_confidence_interval(level = 0.99) 


## -------------------------------------------------------------------------------------------------
# Calculate a 90% bootstrap percentile interval
one_poll_boot %>% 
  get_confidence_interval(level = 0.90) 


## -------------------------------------------------------------------------------------------------
one_poll_boot %>% 
  summarize(ci_endpoints = quantile(stat, c(0.005, 0.025, 0.05, 0.95, 0.975, 0.995)),
            ci_percent = c("99%", "95%", "90%", "90%", "95%", "99%")) -> conf_int_data
###
ggplot(data = conf_int_data, aes(x = ci_percent, y = ci_endpoints)) + 
  geom_line(color = "blue") + 
  theme_bw()

