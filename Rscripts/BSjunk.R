library(infer)
library(tidyverse)
set.seed(37)
gss |> 
  filter(sex == "male") -> MDF
B <- 10^4
xbar <- numeric(B)
for(i in 1:B){
  bss <- sample(MDF$age, size = sum(!is.na(MDF$age)), replace = TRUE)
  xbar[i] <- mean(bss)
}
hist(xbar)
# Or
ggplot(data = tibble(MEAN = xbar), aes(x = MEAN)) + geom_histogram()


MDF |>
  specify(response = age) |>
  generate(reps = 10^4, type = "bootstrap") |>
  calculate(stat = "mean") -> BSD  
BSD
get_confidence_interval(BSD)
get_confidence_interval(BSD, level = .99)
