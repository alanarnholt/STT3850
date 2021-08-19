## Maybe your name and date
## -------------------------------------------------------------------------------------------------
library(ggplot2)
library(NHANES)


## -------------------------------------------------------------------------------------------------
colnames(NHANES)


## -------------------------------------------------------------------------------------------------
# Create bar plot for Home Ownership by Gender
ggplot(NHANES, aes(x = Gender, fill = HomeOwn)) + 
  # Set the position to fill
  geom_bar(position = "fill") +
  ylab("Relative frequencies")


## -------------------------------------------------------------------------------------------------
# Density plot of SleepHrsNight colored by SleepTrouble
ggplot(NHANES, aes(x = SleepHrsNight, color = SleepTrouble)) + 
  # Adjust by 2
  geom_density(adjust = 2) + 
  # Facet by HealthGen
  facet_wrap(vars(HealthGen)) + 
  theme_bw()


## -------------------------------------------------------------------------------------------------
fruits <- c("apple", "banana", "cherry")
fruits %in% c("banana", "cherry")
mean(fruits %in% c("banana", "cherry"))


## -------------------------------------------------------------------------------------------------
library(dplyr)
library(infer)


## -------------------------------------------------------------------------------------------------
homes <- NHANES %>%
  # Select Gender and HomeOwn
  select(Gender, HomeOwn) %>%
  # Filter for HomeOwn equal to "Own" or "Rent"
  filter(HomeOwn %in% c("Own", "Rent"))


## -------------------------------------------------------------------------------------------------
diff_orig <- homes %>%   
  # Group by gender
  group_by(Gender) %>%
  # Summarize proportion of homeowners
  summarize(prop_own = mean(HomeOwn == "Own")) %>%
  # Summarize difference in proportion of homeowners
  summarize(obs_diff_prop = diff(prop_own)) %>% # male - female
  pull()
  
# See the result
diff_orig
##############################
## Another way               #
##############################
#
# First make a new column of numeric data 1 for Own 0 for Rent
#
homes <- homes %>% 
  mutate(HomeOwnN = ifelse(HomeOwn == "Own", 1, 0))
tapply(homes$HomeOwnN, homes$Gender, mean) -> mean_diffs
mean_diffs
obs_mean_diff <- mean_diffs[2] - mean_diffs[1]
names(obs_mean_diff) <- "obs_stat"
obs_mean_diff


## -------------------------------------------------------------------------------------------------
# Specify variables
homeown_perm <- homes %>%
  specify(HomeOwn ~ Gender, success = "Own")

# Print results to console
homeown_perm


## -------------------------------------------------------------------------------------------------
# Hypothesize independence
homeown_perm <- homes %>%
  specify(HomeOwn ~ Gender, success = "Own") %>%
  hypothesize(null = "independence")  

# Print results to console
homeown_perm


## -------------------------------------------------------------------------------------------------
# Perform 10 permutations
homeown_perm <- homes %>%
  specify(HomeOwn ~ Gender, success = "Own") %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 10, type = "permute") 


# Print results to console
homeown_perm


## -------------------------------------------------------------------------------------------------
# Perform 100 permutations
homeown_perm <- homes %>%
  specify(HomeOwn ~ Gender, success = "Own") %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 100, type = "permute") %>% 
  calculate(stat = "diff in props", order = c("male", "female"))
  
# Print results to console
homeown_perm
##############
## Another way to do all of the above
##############
homes <- homes %>% 
  mutate(HomeOwnN = ifelse(HomeOwn == "Own", 1, 0))
tapply(homes$HomeOwnN, homes$Gender, mean) -> mean_diffs
mean_diffs
obs_mean_diff <- mean_diffs[2] - mean_diffs[1]
names(obs_mean_diff) <- "obs_stat"
obs_mean_diff
#################
N <- 100
stat <- numeric(N)
for(i in 1:N){
 stat[i] <- diff(tapply(homes$HomeOwnN, sample(homes$Gender), mean)) 
}
hist(stat)
abline(v = obs_mean_diff, col = "red")
pvalue <- mean(stat <= obs_mean_diff)
pvalue


## -------------------------------------------------------------------------------------------------
# Dotplot of 100 permuted differences in proportions
ggplot(homeown_perm, aes(x = stat)) + 
  geom_dotplot(binwidth = 0.001) + 
  theme_bw()


## -------------------------------------------------------------------------------------------------
REPS <- 1000
homeown_perm <- homes %>%
 specify(HomeOwn ~ Gender, success = "Own") %>%
 hypothesize(null = "independence") %>%
 generate(reps = REPS, type = "permute") %>%
 calculate(stat = "diff in props", order = c("male", "female"))

# Dotplot of 1000 permuted differences in proportions
ggplot(homeown_perm, aes(x = stat)) + 
  geom_density() + 
  theme_bw()
##############
## Another way to do all of the above
##############
homes <- homes %>% 
  mutate(HomeOwnN = ifelse(HomeOwn == "Own", 1, 0))
tapply(homes$HomeOwnN, homes$Gender, mean) -> mean_diffs
mean_diffs
obs_mean_diff <- mean_diffs[2] - mean_diffs[1]
names(obs_mean_diff) <- "obs_stat"
obs_mean_diff
#################
N <- REPS # number of permutations
stat <- numeric(N)
for(i in 1:N){
 stat[i] <- diff(tapply(homes$HomeOwnN, sample(homes$Gender), mean)) 
}
plot(density(stat))
abline(v = obs_mean_diff, col = "red")
pvalue <- mean(stat <= obs_mean_diff)
pvalue


## -------------------------------------------------------------------------------------------------
# Plot permuted differences
ggplot(homeown_perm, aes(x = stat)) + 
  geom_density() +
  geom_vline(xintercept = diff_orig, col = "red") + 
  theme_bw()

# Compare permuted differences to observed difference
homeown_perm %>%
  summarize(n_perm_le_obs = sum(stat <= diff_orig)) %>% 
  pull() -> n_below
n_below

