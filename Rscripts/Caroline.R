

DF <- data.frame(x1 = 1:10, x2 = c(2, NA, NA, 4, 6, NA, 1, 3, NA, NA),
                 x3 = c("in", "in", "out", "out", "in", "in","out",
                        "out", "in", "in"))
DF
library(tidyverse)
DF <- DF %>% 
  filter(x3 == "in") %>% 
  mutate(X2 = ifelse(is.na(x2), 0, x2))
DF
