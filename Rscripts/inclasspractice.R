library(tidyverse)
library(PASWR2)
VIT2005
ggplot(data = VIT2005, aes(x = area, y = totalprice)) + 
  geom_point() + 
  theme_bw()
#
cor(VIT2005$totalprice, VIT2005$area)
