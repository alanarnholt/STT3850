## ---- MEDskip, echo = FALSE----------------------------------------------------------------------------
library(knitr)
knit_hooks$set(document = function(x){
gsub("\\begin{tabular}", "\\medskip{}\\begin{tabular}", x, fixed = TRUE)
})


## ----setup, include = FALSE----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = NA, warning = FALSE, 
                      message = FALSE, fig.align = 'center')


## ------------------------------------------------------------------------------------------------------
library(tidyverse)   # loading collection of packages
library(moderndive)  # datasets and regression functions
library(skimr)       # provides a simple-to-use functions 
                     # for summary statistics
library(gapminder)   # datasets


## ------------------------------------------------------------------------------------------------------
gapminder2007 <- gapminder %>% 
  filter(year == 2007) %>%
  select(country, lifeExp, continent, gdpPercap)
glimpse(gapminder2007)


## ------------------------------------------------------------------------------------------------------
gapminder2007 %>% 
  sample_n(size = 5)


## ---- eval = FALSE-------------------------------------------------------------------------------------
gapminder2007 %>%
  select(lifeExp, continent) %>%
  skim()


## ------------------------------------------------------------------------------------------------------
# Or using summary()
gapminder2007 %>% 
  select(lifeExp, continent) %>%
  summary()


## ----out.height = '30%', out.width = '40%'-------------------------------------------------------------
ggplot(gapminder2007, aes(x = lifeExp)) +
  geom_histogram(binwidth=5, color = "blue", fill = "lightblue") +
  labs(x = "Life expectancy", y = "Number of countries",
       title = "Histogram of distribution of worldwide 
       life expectancies") +
  theme_bw()


## ----out.height = '45%', out.width = '80%'-------------------------------------------------------------
ggplot(gapminder2007, aes(x = lifeExp)) +
  geom_histogram(binwidth = 5, color = "blue",
                 fill = "lightblue") +
  labs(x = "Life expectancy", 
       y = "Number of countries",
       title = "Histogram of distribution of worldwide 
       life expectancies") +
  facet_wrap(vars(continent), nrow = 2) + 
  theme_bw()


## ----out.height = '50%',out.width = '80%'--------------------------------------------------------------
ggplot(gapminder2007, aes(x = continent, y = lifeExp)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Continent", y = "Life expectancy",
       title = "Life expectancy by continent") + 
  theme_bw()


## ------------------------------------------------------------------------------------------------------
lifeExp_by_continent <- gapminder2007 %>%
  group_by(continent) %>%
  summarize(median = median(lifeExp), 
            mean = mean(lifeExp)) %>% 
  mutate(`Difference versus Africa` = mean - mean[1])
knitr::kable(lifeExp_by_continent)


## ------------------------------------------------------------------------------------------------------
lifeExp_model <- lm(lifeExp ~ continent, data = gapminder2007)
knitr::kable(get_regression_table(lifeExp_model))
summary(lifeExp_model)

## ------------------------------------------------------------------------------------------------------
gapminder2007$continent <- relevel(gapminder2007$continent, ref='Americas')
lifeExp_model1 <- lm(lifeExp ~ continent, data = gapminder2007)
knitr::kable(get_regression_table(lifeExp_model1))


## ------------------------------------------------------------------------------------------------------
regression_points <- get_regression_points(lifeExp_model, 
                                           ID = "country") 
knitr::kable(regression_points %>% head(n = 9))


## ------------------------------------------------------------------------------------------------------
library(tidyverse) 
library(moderndive)
library(skimr)
library(ISLR)


## ------------------------------------------------------------------------------------------------------
evals_ch6 <- evals %>%
  select(ID, score, age, gender)


## ------------------------------------------------------------------------------------------------------
glimpse(evals_ch6)
# Or
evals_ch6 %>% 
  sample_n(size = 2)


## ----eval = FALSE--------------------------------------------------------------------------------------
## evals_ch6 %>%
##   select(score, age, gender) %>%
##   skim()


## ------------------------------------------------------------------------------------------------------
# Or
evals_ch6 %>% 
  select(score, age, gender) %>% 
  summary()


## ------------------------------------------------------------------------------------------------------
evals_ch6 %>% 
  summarize(r = cor(score, age))
# or using the get_correlation wrapper
# from moderndive
evals_ch6 %>% 
  get_correlation(score ~ age)


## ----eval = FALSE--------------------------------------------------------------------------------------
## ggplot(evals_ch6, aes(x = age, y = score, color = gender)) +
##   geom_point() +
##   labs(x = "Age", y = "Teaching Score", color = "Gender",
##        title = "Interaction Model") +
##   geom_smooth(method = "lm", se = FALSE) +
##   theme_bw() -> int_mod
## int_mod


## ----echo = FALSE, label = "interaction", fig.cap = "Interaction Model", out.height = '90%', out.width = '90%'----
ggplot(evals_ch6, aes(x = age, y = score, color = gender)) +
  geom_point() +
  labs(x = "Age", y = "Teaching Score", color = "Gender",
       title = "Interaction Model") +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() -> int_mod
int_mod


## ------------------------------------------------------------------------------------------------------
# Fit regression model:
score_model_interaction <- lm(score ~ age + gender + age:gender, 
                              data = evals_ch6)
# Get regression table:
knitr::kable(get_regression_table(score_model_interaction))


## ----echo = FALSE, out.height = '30%', out.width = '60%'-----------------------------------------------
knitr::include_graphics("week5_2.png")


## ----eval = FALSE--------------------------------------------------------------------------------------
## ggplot(evals_ch6, aes(x = age, y = score, color = gender)) +
##   geom_point() +
##   labs(x = "Age", y = "Teaching Score", color = "Gender",
##        title = "Parallel Slopes Model") +
##   geom_parallel_slopes(se = FALSE) +
##   theme_bw() -> ps_mod
## ps_mod


## ----echo = FALSE, out.height = '90%', out.width = '90%'-----------------------------------------------
ggplot(evals_ch6, aes(x = age, y = score, color = gender)) +
  geom_point() +
  labs(x = "Age", y = "Teaching Score", color = "Gender",
       title = "Parallel Slopes Model") +
  geom_parallel_slopes(se = FALSE) + 
  theme_bw() -> ps_mod
ps_mod


## ------------------------------------------------------------------------------------------------------
# Fit regression model:
score_model_parallel_slopes <- lm(score ~ age + gender, 
                                  data = evals_ch6)
# Get regression table:
knitr::kable(get_regression_table(score_model_parallel_slopes))


## ----echo = FALSE, out.height = '20%', out.width = '60%'-----------------------------------------------
knitr::include_graphics("week5_3.png")


## ---- echo = FALSE, out.height = '90%', out.width = '90%'----------------------------------------------
library(patchwork)
int_mod + ps_mod


## ----echo = FALSE, out.height = '45%',out.width = '80%', fig.align='center'----------------------------
# knitr::include_graphics("week5_5.png")
int_mod +
  geom_vline(xintercept = 36, linetype = "dashed") + 
  geom_vline(xintercept = 59, linetype = "dashed")


## ------------------------------------------------------------------------------------------------------
predict(score_model_interaction, 
        newdata = data.frame(age = 36, gender = "female"))
predict(score_model_interaction, 
        newdata = data.frame(age = 59, gender = "male"))

predict(score_model_interaction, 
        newdata = data.frame(age = c(36, 59), 
                             gender = c("female","male")),
        interval = "pred")


## ----out.height = '40%', out.width = '70%'-------------------------------------------------------------
library(PASWR2)
VIT2005 <- VIT2005 %>% 
  mutate(elevator = factor(elevator, labels = c("No", "Yes")))
ggplot(data = VIT2005, aes(x = area, y = totalprice)) + 
  geom_point() + 
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE)


## ------------------------------------------------------------------------------------------------------
mod_simple <- lm(totalprice ~ area, data = VIT2005)
summary(mod_simple)


## ------------------------------------------------------------------------------------------------------
knitr::kable(get_regression_table(mod_simple))


## ----out.height = '50%', out.width = '80%'-------------------------------------------------------------
ggplot(VIT2005, aes(x = area, y = totalprice, color = elevator)) +
  geom_point() +
  labs(x = "Area (sq meters)", y = "Total Price (euros)", 
       color = "Elevator") +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()


## ------------------------------------------------------------------------------------------------------
mod_int <- lm(totalprice ~ area + elevator + area:elevator, data = VIT2005)
summary(mod_int)


## ------------------------------------------------------------------------------------------------------
mod_ps <- lm(totalprice ~ area + elevator, data = VIT2005)
summary(mod_ps)


## ----out.height = '50%', out.width = '80%'-------------------------------------------------------------
library(ggfortify)
autoplot(mod_int, ncol = 2, nrow = 1, which = 1:2) + 
  theme_bw()


## ------------------------------------------------------------------------------------------------------
library(ISLR)
credit_ch6 <- Credit %>% 
  as_tibble() %>% 
  select(ID, debt = Balance, credit_limit = Limit, 
         income = Income, credit_rating = Rating, age = Age)
glimpse(credit_ch6)


## ------------------------------------------------------------------------------------------------------
credit_ch6 %>% 
  sample_n(size = 5)


## ---- eval = FALSE-------------------------------------------------------------------------------------
## credit_ch6 %>%
##   select(debt, credit_limit, income) %>%
##   skim()


## ------------------------------------------------------------------------------------------------------
credit_ch6 %>% 
  select(debt, credit_limit, income) %>% 
  summary()


## ------------------------------------------------------------------------------------------------------
credit_ch6 %>% 
  select(debt, credit_limit, income) %>% 
  cor()


## ----out.height = '45%', out.width = '60%'-------------------------------------------------------------
library(psych)
pairs.panels(credit_ch6[, 2:4],  # select debt (2), credit_limit (3), 
             # income (4)
             method = "pearson", # correlation method
             hist.col = "lightblue",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
             )


## ----eval = FALSE--------------------------------------------------------------------------------------
## ggplot(data = credit_ch6, aes(x = credit_limit, y = debt)) +
##   geom_point() +
##   labs(x= "Credit limit (in$)", y = "Credit card debt (in$)",
##        title = "Debt and Credit Limit") +
##   geom_smooth(method = "lm", se = FALSE) +
##   theme_bw() -> p1
## ggplot(data = credit_ch6, aes(x = income, y = debt)) +
##   geom_point() +
##   labs(x = "Income (in $1000)", y = "Credit card debt (in $)",
##        title = "Debt and Income") +
##   geom_smooth(method = "lm", se = FALSE) +
##   theme_bw() -> p2
## library(patchwork)
## p1 + p2


## ----echo = FALSE, out.height = '90%', out.width = '90%'-----------------------------------------------
ggplot(data = credit_ch6, aes(x = credit_limit, y = debt)) + 
  geom_point() + 
  labs(x= "Credit limit (in$)", y = "Credit card debt (in$)", 
       title = "Debt and Credit Limit") + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() -> p1
ggplot(data = credit_ch6, aes(x = income, y = debt)) + 
  geom_point() + 
  labs(x = "Income (in $1000)", y = "Credit card debt (in $)",
       title = "Debt and Income") +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() -> p2
library(patchwork)
p1 + p2


## ----eval = FALSE--------------------------------------------------------------------------------------
## library(plotly)
## p <- plot_ly(data = credit_ch6, z = ~debt, x = ~credit_limit,
##              y = ~income) %>% add_markers()
## mod <- lm(debt ~ credit_limit + income, data = credit_ch6)
## x <- seq(min(credit_ch6$credit_limit),
##          max(credit_ch6$credit_limit), length = 70)
## y <- seq(min(credit_ch6$income),
##          max(credit_ch6$income), length = 70)
## plane <- outer(x, y, function(a, b){coef(mod)[1] +
##                coef(mod)[2]*a + coef(mod)[3]*b})
## # draw the plane
## p %>%
##   add_surface(x = ~x, y = ~y, z = ~plane)


## ----echo = FALSE, warning=FALSE, message=FALSE,out.height = '50%',out.width = '70%', fig.align='center'----
knitr::include_graphics("week5_7.png")


## ------------------------------------------------------------------------------------------------------
# Fit regression model:
debt_model <- lm(debt ~ credit_limit + income, 
                 data = credit_ch6)
# Get regression table:
knitr::kable(get_regression_table(debt_model))     


## ----out.height = '50%', out.width = '80%'-------------------------------------------------------------
autoplot(debt_model, ncol = 2, nrow = 1, which = 1:2) + 
  theme_bw()


## ----echo=TRUE,warning=FALSE, message=FALSE,out.height = '40%',out.width = '80%', fig.align='center'----
library(ISLR)
credit_paradox <- Credit %>% 
  select(ID, debt = Balance, credit_limit = Limit, 
         credit_rating = Rating, income = Income, age = Age)
ggplot(data = credit_paradox, aes(x = credit_limit, y = debt)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() -> p1
ggplot(data = credit_paradox, aes(x = income, y = debt)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() -> p2
library(patchwork)
p1 + p2


## ------------------------------------------------------------------------------------------------------
mod <- lm(debt ~ credit_limit + income, data = credit_paradox)
summary(mod)$coef


## ------------------------------------------------------------------------------------------------------
qs <- quantile(credit_paradox$credit_limit, probs = seq(0, 1, .25))
credit_paradox <- credit_paradox %>% 
  mutate(credit_cats = cut(credit_limit, breaks = qs, 
                           include.lowest = TRUE))
knitr::kable(head(credit_paradox))


## ----out.height = '50%', out.width = '80%'-------------------------------------------------------------
ggplot(data = credit_paradox, aes(x = credit_limit)) +
  geom_density(fill = "pink", color = "black") + 
  geom_vline(xintercept = qs, color = "blue", 
             linetype = "dashed") + 
  theme_bw()


## ------------------------------------------------------------------------------------------------------
credit_paradox %>% 
  group_by(credit_cats) %>% 
  summarize(n())



## ----eval = FALSE--------------------------------------------------------------------------------------
## p1 <- ggplot(data = credit_paradox, aes(x = income, y = debt)) +
##   geom_point() +
##   geom_smooth(method = "lm", se = FALSE) +
##   theme_bw() +
##   labs(y = "Credit card debt (in $)",
##        x = "Income (in $1000)")
## p2 <- ggplot(data = credit_paradox, aes(x = income, y = debt,
##                                         color = credit_cats)) +
##   geom_point() +
##   geom_smooth(method = "lm", se = FALSE) +
##   theme_bw() +
##   labs(y = "Credit card debt (in $)",
##        x = "Income (in $1000)",
##        color = "Credit limit bracket")
## p1 + p2


## ----echo = FALSE, out.height = '90%', out.width = '90%'-----------------------------------------------
p1 <- ggplot(data = credit_paradox, aes(x = income, y = debt)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() + 
  labs(y = "Credit card debt (in $)",
       x = "Income (in $1000)")
p2 <- ggplot(data = credit_paradox, aes(x = income, y = debt, color = credit_cats)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() + 
  labs(y = "Credit card debt (in $)",
       x = "Income (in $1000)",
       color = "Credit limit bracket")
p1 + p2

