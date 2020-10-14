## -------------------------------------------------------------------------------------------------
library(tidyverse)
disc <- data.frame(promote = c(rep("promoted", 35), 
                               rep("not_promoted", 13)),     
                   sex = c(rep("male", 21), rep("female", 14),
                           rep("male", 3), rep("female", 10)))
disc %>%    
  group_by(sex) %>%     
  summarize(promoted_prop = mean(promote == "promoted"))                    


## -------------------------------------------------------------------------------------------------
# Create a contingency table summarizing the data
disc %>%
  # Count the rows by sex, promote
  count(sex, promote)

# Find proportion of each sex who were promoted
disc %>%
  # Group by sex
  group_by(sex) %>%
  # Calculate proportion promoted summary stat
  summarize(promoted_prop = mean(promote == "promoted"))


## -------------------------------------------------------------------------------------------------
library(infer)
# Replicate the entire data frame, permuting the promote variable
disc_perm <- disc %>%
  specify(promote ~ sex, success = "promoted") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 5, type = "permute")

disc_perm %>%
  # Group by replicate
  group_by(replicate) %>%
  # Count per group
  count()

disc_perm %>%
  # Calculate difference in proportion, male then female
  calculate(stat = "diff in props", order = c("male", "female"))


## -------------------------------------------------------------------------------------------------
# Calculate the observed difference in promotion rate
diff_orig <- disc %>%
  # Group by sex
  group_by(sex) %>%
  # Summarize to calculate fraction promoted
  summarize(prop_prom = mean(promote == "promoted")) %>%
  # Summarize to calculate difference
  summarize(stat = diff(prop_prom)) %>% 
  pull()
    
# See the result
diff_orig


## -------------------------------------------------------------------------------------------------
# Create data frame of permuted differences in promotion rates
disc_perm <- disc %>%
  # Specify promote vs. sex
  specify(promote ~ sex, success = "promoted") %>%
  # Set null hypothesis as independence
  hypothesize(null = "independence") %>%
  # Generate 1000 permutations
  generate(reps = 1000, type = "permute") %>%
  # Calculate difference in proportions
  calculate(stat = "diff in props", order = c("male", "female"))


## -------------------------------------------------------------------------------------------------
# Using permutation data, plot stat
ggplot(disc_perm, aes(x = stat)) + 
  # Add a histogram layer
  geom_histogram(binwidth = 0.01) +
  # Add a vertical line at diff_orig
  geom_vline(aes(xintercept = diff_orig), color = "red")


## -------------------------------------------------------------------------------------------------
# Find the 0.90, 0.95, and 0.99 quantiles of diff_perm
disc_perm %>% 
  summarize(q.90 = quantile(stat, probs = 0.90),
            q.95 = quantile(stat, probs = 0.95),
            q.99 = quantile(stat, probs = 0.99))


## -------------------------------------------------------------------------------------------------
# Find the 0.01, 0.05, and 0.10 quantiles of diff_perm
disc_perm %>% 
  summarize(q.01 = quantile(stat, probs = 0.01),
            q.05 = quantile(stat, probs = 0.05),
            q.10 = quantile(stat, probs = 0.10))


## -------------------------------------------------------------------------------------------------
# Creating the datasets disc_small and disc_big
disc_small <- data.frame(promote = c(rep("promoted", 12), 
                               rep("not_promoted", 4)),     
                   sex = c(rep("male", 7), rep("female", 5),
                           rep("male", 1), rep("female", 3)))
diff_orig_small <- disc_small %>% 
  group_by(sex) %>% 
summarize(prop_prom = mean(promote =="promoted")) %>% 
  summarize(stat = diff(prop_prom)) %>% 
  pull()
diff_orig_small
disc_big <- data.frame(promote = c(rep("promoted", 350), 
                               rep("not_promoted", 130)),     
                   sex = c(rep("male", 210), rep("female", 140),
                           rep("male", 30), rep("female", 100)))
diff_orig_big <- disc_big %>% 
  group_by(sex) %>% 
summarize(prop_prom = mean(promote =="promoted")) %>% 
  summarize(stat = diff(prop_prom)) %>% 
  pull()
diff_orig_big


## -------------------------------------------------------------------------------------------------
# Creating disc_perm_small and disc_perm_big
set.seed(123)
disc_perm_small <- disc_small %>% 
  specify(promote ~ sex, success = "promoted") %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in props", order = c("male", "female"))
disc_perm_big <- disc_big %>% 
  specify(promote ~ sex, success = "promoted") %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in props", order = c("male", "female"))


## -------------------------------------------------------------------------------------------------
# Tabulate the small dataset
disc_small %>% 
  # Select sex and promote
  count(sex, promote)


## -------------------------------------------------------------------------------------------------
# Do the same for disc_big
disc_big %>%
  count(sex, promote)


## -------------------------------------------------------------------------------------------------
# Using disc_perm_small, plot stat
ggplot(disc_perm_small, aes(x = stat)) + 
  # Add a histogram layer with binwidth 0.01
  geom_histogram(binwidth = 0.01) +
  # Add a vline layer, crossing x-axis at diff_orig_small
  geom_vline(aes(xintercept = diff_orig_small), color = "red") + 
  theme_bw()


## -------------------------------------------------------------------------------------------------
# Swap the dataset to disc_perm_big
ggplot(disc_perm_big, aes(x = stat)) + 
  geom_histogram(binwidth = 0.01) +
  # Change the x-axis intercept to diff_orig_big
  geom_vline(aes(xintercept = diff_orig_big), color = "red") + 
  theme_bw()


## -------------------------------------------------------------------------------------------------
calc_upper_quantiles <- function(dataset) {
  dataset %>% 
    summarize(
      q.90 = quantile(stat, p = 0.90),
      q.95 = quantile(stat, p = 0.95),
      q.99 = quantile(stat, p = 0.99)
    )
}


## -------------------------------------------------------------------------------------------------
# Recall the quantiles associated with the original dataset
calc_upper_quantiles(disc_perm)


## -------------------------------------------------------------------------------------------------
# Calculate the quantiles associated with the small dataset
calc_upper_quantiles(disc_perm_small)


## -------------------------------------------------------------------------------------------------
# Calculate the quantiles associated with the big dataset
calc_upper_quantiles(disc_perm_big)


## -------------------------------------------------------------------------------------------------
# Visualize and calculate the p-value for the original dataset
disc_perm %>%
  visualize(obs_stat = diff_orig, direction = "greater") + 
  theme_bw()
#
orig_p <- disc_perm %>%
  get_p_value(obs_stat = diff_orig, direction = "greater") %>% 
  pull()
orig_p


## -------------------------------------------------------------------------------------------------
# Visualize and calculate the p-value for the small dataset
disc_perm_small %>%
  visualize(obs_stat = diff_orig_small, direction = "greater") + 
  theme_bw()

small_p <- disc_perm_small %>%
  get_p_value(obs_stat = diff_orig_small, direction = "greater") %>% 
  pull()
small_p


## -------------------------------------------------------------------------------------------------
# Visualize and calculate the p-value for the big dataset
disc_perm_big %>%
  visualize(obs_stat = diff_orig_big, direction = "greater") + 
  theme_bw()

big_p <- disc_perm_big %>%
  get_p_value(obs_stat = diff_orig_big, direction = "greater") %>% 
  pull()
big_p


## -------------------------------------------------------------------------------------------------
disc_new <- data.frame(promote = c(rep("promoted", 35), 
                               rep("not_promoted", 13)),     
                   sex = c(rep("male", 18), rep("female", 17),
                           rep("male", 6), rep("female", 7)))
disc_new %>%    
  group_by(sex) %>%     
  summarize(promoted_prop = mean(promote == "promoted"))
#
diff_orig_new <- disc_new %>% 
  group_by(sex) %>% 
summarize(prop_prom = mean(promote =="promoted")) %>% 
  summarize(stat = diff(prop_prom)) %>% 
  pull()
diff_orig_new
#
# Create data frame of permuted differences in promotion rates
disc_perm_new <- disc_new %>%
  # Specify promote vs. sex
  specify(promote ~ sex, success = "promoted") %>%
  # Set null hypothesis as independence
  hypothesize(null = "independence") %>%
  # Generate 1000 permutations
  generate(reps = 1000, type = "permute") %>%
  # Calculate difference in proportions
  calculate(stat = "diff in props", order = c("male", "female"))


## -------------------------------------------------------------------------------------------------
# Recall the original data
disc %>% 
  count(sex, promote)


## -------------------------------------------------------------------------------------------------
# Tabulate the new data
disc_new %>%
  count(sex, promote)


## -------------------------------------------------------------------------------------------------
# Recall the distribution of the original permuted differences
ggplot(disc_perm, aes(x = stat)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = diff_orig), color = "red") + 
  theme_bw()


## -------------------------------------------------------------------------------------------------
# Plot the distribution of the new permuted differences
ggplot(disc_perm_new, aes(x = stat)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = diff_orig_new), color = "red") + 
  theme_bw()


## -------------------------------------------------------------------------------------------------
# Recall the p-value from the original data
disc_perm %>%
  summarize(p_value = mean(stat >= diff_orig))


## -------------------------------------------------------------------------------------------------
# Find the p-value from the new data
disc_perm_new %>%
  summarize(p_value = mean(stat >= diff_orig_new))


## -------------------------------------------------------------------------------------------------
# Calculate the two-sided p-value
disc_perm %>%
  summarize(p_value = mean(stat >= diff_orig)*2)

