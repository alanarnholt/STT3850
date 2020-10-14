## -------------------------------------------------------------------------------------------------
library(tidyverse)
library(infer)
# Creating dataset opportunity
opportunity <- data.frame(decision = c(rep("buyDVD", 97), 
                               rep("nobuyDVD", 53)),     
                   group = c(rep("control", 56), rep("treatment", 41),
                           rep("control", 19), rep("treatment", 34)))


## -------------------------------------------------------------------------------------------------
# Tabulate the data
opportunity %>%
  count(decision, group)


## -------------------------------------------------------------------------------------------------
# Find the proportion who bought the DVD in each group
opportunity %>%
  group_by(group) %>%
  summarize(buy_prop = mean(decision == "buyDVD"))


## -------------------------------------------------------------------------------------------------
# Create a barplot
ggplot(opportunity, aes(x = group, fill = decision)) + 
  geom_bar(position = "fill") + 
  theme_bw()


## -------------------------------------------------------------------------------------------------
# Calculate the observed difference in purchase rate
diff_obs <- opportunity %>%
  # Group by group
  group_by(group) %>%
  # Calculate proportion deciding to buy a DVD
  summarize(prop_buy = mean(decision == "buyDVD")) %>%
  # Calculate difference between groups
  summarize(stat = diff(prop_buy)) %>% 
  pull
diff_obs


## -------------------------------------------------------------------------------------------------
# Create data frame of permuted differences in purchase rates
opp_perm <- opportunity %>%
  # Specify decision vs. group, where success is buying a DVD
  specify(decision ~ group, success = "buyDVD") %>%
  # Set the null hypothesis to independence
  hypothesize(null = "independence") %>%
  # Generate 1000 reps of type permute
  generate(reps = 1000, type = "permute") %>%
  # Calculate the summary stat difference in proportions
  calculate(stat = "diff in props", order = c("treatment", "control"))
    
# Review the result
opp_perm


## -------------------------------------------------------------------------------------------------
# Using the permuation data, plot stat
ggplot(opp_perm, aes(x = stat)) + 
  # Add a histogram layer with binwidth 0.005
  geom_histogram(binwidth = 0.005) +
  # Add a vline layer with intercept diff_obs
  geom_vline(aes(xintercept = diff_obs), color = "red") + 
  theme_bw()


## -------------------------------------------------------------------------------------------------
# Visualize the statistic 
opp_perm %>%
  visualize() +
  shade_p_value(obs_stat = diff_obs, direction = "less") + 
  theme_bw()


## -------------------------------------------------------------------------------------------------
# Calculate the p-value using `get_p_value`
opp_perm %>%
  get_p_value(obs_stat = diff_obs, direction = "less")


## -------------------------------------------------------------------------------------------------
# Calculate the p-value using `summarize`
p_val <- opp_perm %>%
  summarize(p_value = mean(stat <= diff_obs)) %>% 
  pull()
p_val


## ---- eval = FALSE--------------------------------------------------------------------------------
## opp_perm %>%
##   summarize(p_value = mean(stat <= diff_obs))}


## -------------------------------------------------------------------------------------------------
# Calculate the two-sided p-value
opp_perm %>%
  summarize(p_value = mean(stat <= diff_obs)*2)

