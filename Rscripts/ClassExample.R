library(tidyverse)
library(PASWR2)
?VIT2005
ggplot(data = VIT2005, aes(x = area, y = totalprice)) +
  geom_point(color = "orange", size = 5, alpha = .2) + 
  theme_bw()
cor(VIT2005$totalprice, VIT2005$area)
VIT2005 %>% 
  summarize(r = cor(totalprice, area))
VIT2005 %>% 
  mutate(z_x = (area - mean(area))/sd(area), 
         z_y = (totalprice - mean(totalprice))/sd(totalprice)) %>% 
  select(area, totalprice, z_x, z_y) -> JU
JU
JU %>% 
  summarize(r = sum(z_x*z_y)/217)

