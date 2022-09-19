library(tidyverse)
library(PASWR2)
ggplot(data = GRADES, aes(x = sat, y = gpa)) +
  geom_point()
