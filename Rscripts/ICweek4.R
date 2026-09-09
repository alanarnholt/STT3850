# 
library(tidymodels)
ames
# Characterize the distribution of Sale_Price

## Create a scatter plot of Sale_Price vs Gr_Liv_Area
ggplot(data = ames, aes(x = Gr_Liv_Area, y = Sale_Price)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# Create a new variable log10(Sale_Price) and log10(Gr_Liv_Area)

ames |> 
  mutate(Log10Sale_Price = log10(Sale_Price), Log10Gr_Liv_Area = log10(Gr_Liv_Area)) |> 
  relocate(Log10Sale_Price, .after = MS_SubClass) -> ames2

# Characterize the distribution of Log10Sale_Price

ggplot(data = ames2, aes(x = Log10Sale_Price)) + 
  geom_density()

# Create a scatter plot of Log10Sale_Price versus Log10Gr_Liv_Area

ggplot(data = ames2, aes(x = Log10Gr_Liv_Area, y = Log10Sale_Price)) + 
  geom_point()

# Compute the correalation between Log10Gr_Liv_Area and Log10Sale_Price

cor(ames2$Log10Gr_Liv_Area, ames2$Log10Sale_Price)

# regress Log10Sale_Price onto Log10Gr_Liv_Area

mod_lm <- lm(Log10Sale_Price ~ Log10Gr_Liv_Area, data = ames2)
summary(mod_lm)

ggplot(data = ames2, aes(x = Log10Gr_Liv_Area, y = Log10Sale_Price)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)



