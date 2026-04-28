library(PASWR2)
mini <- VIT2005 |> filter(age <= 10)
mini <- mini |> 
  mutate(toilets = as.factor(toilets))

ggplot(data = mini, aes(x = toilets, y = totalprice)) +
  geom_boxplot()

library(infer)
set.seed(123)


\mini |> 
  specify(totalprice ~ toilets) |> 
  hypothesize(null = "independence") |> 
  generate(reps = 10^4, type = "permute") |> 
  calculate(stat = "diff in medians") -> null 
visualize(null)
