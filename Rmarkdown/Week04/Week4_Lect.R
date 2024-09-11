## ----MEDskip, echo = FALSE--------------------------------------------------------------------------------------------------
library(knitr)
knit_hooks$set(document = function(x){
gsub("\\begin{tabular}", "\\medskip{}\\begin{tabular}", x, fixed = TRUE)
})


## ----setup, include = FALSE-------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = NA, warning = FALSE, message = FALSE, fig.align = 'center')


## ---------------------------------------------------------------------------------------------------------------------------
library(ggplot2)    #  for data visualization
library(dplyr)      #  for data wrangling
library(readr)      #  for importing spreadsheet data into R
library(moderndive) #  datasets and regression functions
library(skimr)      #  provides simple-to-use functions 
                    #  for summary statistics


## ---------------------------------------------------------------------------------------------------------------------------
evals_ch5 <- evals |>
  select(ID, score, bty_avg, age)   # take subset
glimpse(evals_ch5)


## ---------------------------------------------------------------------------------------------------------------------------
evals_ch5 |>
  sample_n(size = 5)


## ---------------------------------------------------------------------------------------------------------------------------
evals_ch5 |>
  summarize(mean_bty_avg = mean(bty_avg),
            mean_score = mean(score),
            median_bty_avg = median(bty_avg), 
            median_score = median(score))


## ----eval = FALSE-----------------------------------------------------------------------------------------------------------
library(skimr)
evals_ch5 |>
  select(score, bty_avg) |>
  skim()


## ----echo = FALSE, out.height = '45%', out.width = '90%'--------------------------------------------------------------------
knitr::include_graphics("week4_2.png")


## ---------------------------------------------------------------------------------------------------------------------------
library(BSDA) 
head(Gpa)


## ----out.height = '50%',out.width = '50%'-----------------------------------------------------------------------------------
ggplot(data = Gpa, aes(x = hsgpa, y = collgpa)) + 
  labs(x = "High School GPA", y = "College GPA") +
  geom_point(size = 5, color = "blue") + 
  theme_bw()


## ---------------------------------------------------------------------------------------------------------------------------
values <- Gpa |> 
  mutate(y_ybar = collgpa - mean(collgpa), 
         x_xbar = hsgpa - mean(hsgpa),
         zx = x_xbar/sd(hsgpa), zy = y_ybar/sd(collgpa))
values


## ---------------------------------------------------------------------------------------------------------------------------
values |> 
  summarize(r = (1/9)*sum(zx*zy))


## ---------------------------------------------------------------------------------------------------------------------------
Gpa |> 
  summarize(r = cor(collgpa, hsgpa))


## ---------------------------------------------------------------------------------------------------------------------------
Gpa |> 
  get_correlation(formula = collgpa ~ hsgpa)


## ----out.height = '45%', out.width = '60%'----------------------------------------------------------------------------------
p1 <- ggplot(data = Gpa, aes(x = hsgpa, y = collgpa)) + 
  geom_point(size = 5, color = "red") + 
  theme_bw()
p2 <- ggplot(data = values, aes(x = zx, y = zy)) + 
  geom_point(size = 5, color = "blue") + 
  theme_bw()
library(patchwork)
p1/p2


## ---------------------------------------------------------------------------------------------------------------------------
evals_ch5 |> 
  get_correlation(formula = score ~ bty_avg)


## ---------------------------------------------------------------------------------------------------------------------------
evals_ch5 |> 
  summarize(correlation = cor(score, bty_avg))


## ----out.height = '45%', out.width = '60%'----------------------------------------------------------------------------------
ggplot(evals_ch5, aes(x = bty_avg, y = score)) +
  geom_jitter(size = 3, color = "blue") +
  labs(x = "Beauty Score", y = "Teaching Score",
       title = "Scatterplot teaching and beauty scores") +
  theme_bw()


## ----out.height = '45%',out.width = '60%'-----------------------------------------------------------------------------------
ggplot(evals_ch5, aes(x = bty_avg, y = score)) +
  geom_point(size = 3, color = "purple") +
  labs(x = "Beauty Score", y = "Teaching Score",
       title = "Teaching and Beauty Scores") +  
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()


## ----echo = FALSE, out.height = '45%', out.width = '60%'--------------------------------------------------------------------
ggplot(evals_ch5, aes(x = bty_avg, y = score)) +
  geom_point(size = 3, color = "purple") +
  labs(x = "Beauty Score", y = "Teaching Score",
       title = "Teaching and Beauty Scores") +  
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()


## ---------------------------------------------------------------------------------------------------------------------------
# Fit regression model:
score_model <- lm(score ~ bty_avg, data = evals_ch5)
# Get regression table:
get_regression_table(score_model)


## ---------------------------------------------------------------------------------------------------------------------------
# Using summary()
summary(score_model)


## ---------------------------------------------------------------------------------------------------------------------------
# Use formula
evals_ch5 |> 
  summarize(b1 = cor(bty_avg, score)*sd(score)/sd(bty_avg),
            b0 = mean(score) - b1*mean(bty_avg))


## ----echo=TRUE,warning=FALSE, message=FALSE,out.height = '50%',out.width = '50%', fig.align='center'------------------------
# Fit regression model:
evals_ch5[21,]
evals_ch5[21,]$bty_avg


## ----echo = FALSE, out.height = '40%', out.width = '80%'--------------------------------------------------------------------
knitr::include_graphics("week4_4.png")


## ---------------------------------------------------------------------------------------------------------------------------
regression_points <- get_regression_points(score_model)
regression_points


## ----echo = FALSE, out.width = '90%', fig.pos="h"---------------------------------------------------------------------------
rl <- lapply(list("week6_3a.png", "week6_3b.png","week6_3c.png","week6_3d.png"), png::readPNG)
gl <- lapply(rl, grid::rasterGrob)
do.call(gridExtra::grid.arrange, gl)


## ----out.height = '60%', out.width = '80%'----------------------------------------------------------------------------------
library(ggfortify)
autoplot(score_model, ncol = 2, nrow = 1, which = 1:2) +
  theme_bw()


## ---------------------------------------------------------------------------------------------------------------------------
# Fit regression model:
score_model <- lm(score ~ bty_avg, data = evals_ch5)
# Get regression table:
get_regression_table(score_model, conf.level = 0.95) 
# conf.level = 0.95 is default


## ---------------------------------------------------------------------------------------------------------------------------
summary(score_model)


## ---------------------------------------------------------------------------------------------------------------------------
eis <- resid(score_model)
RSS <- sum(eis^2)
RSS
RSE <- sqrt(RSS/(dim(evals_ch5)[1]-2))
RSE
# Or
summary(score_model)$sigma


## ---------------------------------------------------------------------------------------------------------------------------
b0 <- coef(score_model)[1]
b1 <- coef(score_model)[2]
c(b0, b1)
XTXI <- summary(score_model)$cov.unscaled
MSE <- summary(score_model)$sigma^2
(var_cov_b <- MSE*XTXI)


## ---------------------------------------------------------------------------------------------------------------------------
seb0 <- sqrt(var_cov_b[1, 1])
seb1 <- sqrt(var_cov_b[2, 2])
c(seb0, seb1)
# confidence interval
(df <- dim(evals_ch5)[1] - 2)
##b0
t_critical <- qt(0.975, df)
c(b0 - t_critical*seb0, b0 + t_critical*seb0)


## ---------------------------------------------------------------------------------------------------------------------------
##b1
c(b1 - t_critical*seb1, b1 + t_critical*seb1)
# Or
confint(score_model, level = 0.95)
# Testing
tb0 <- b0/seb0
tb1 <- b1/seb1


## ---------------------------------------------------------------------------------------------------------------------------
c(tb0, tb1)
pvalues <- c(pt(tb0, df, lower = FALSE)*2, 
             pt(tb1, df, lower = FALSE)*2)
pvalues
summary(score_model)$coef[ ,4]


## ----echo = FALSE, out.width = '65%'----------------------------------------------------------------------------------------
knitr::include_graphics("week2_5.png")


## ----echo=FALSE, out.width = '60%'------------------------------------------------------------------------------------------
knitr::include_graphics("week2_7.png")


## ---------------------------------------------------------------------------------------------------------------------------
TSS <- sum((evals_ch5$score - mean(evals_ch5$score))^2)
c(RSS, TSS)
R2 <- (TSS - RSS)/TSS
R2
# Or
summary(score_model)$r.squared


## ---------------------------------------------------------------------------------------------------------------------------
get_regression_points(score_model) 


## ---------------------------------------------------------------------------------------------------------------------------
get_regression_points(score_model) |> 
  summarize(var_y = var(score), 
            var_y_hat = var(score_hat), 
            var_residual = var(residual)) |> 
  mutate(R2 = var_y_hat/var_y)


## ----eval = FALSE-----------------------------------------------------------------------------------------------------------
## PIM <- predict(score_model, interval = "pred")
## df1 <- cbind(evals_ch5, PIM)
## ggplot(data = df1, aes(x = bty_avg, y = score)) +
##   geom_point() +
##   geom_smooth(method = "lm") +
##   geom_line(aes(y = upr), color = "purple",
##             linetype = "dashed") +
##   geom_line(aes(y = lwr), color = "purple",
##             linetype = "dashed") +
##   theme_bw() -> p1
## p1


## ----echo = FALSE, out.height = '70%', out.width = '70%'--------------------------------------------------------------------
PIM <- predict(score_model, interval = "pred")
df1 <- cbind(evals_ch5, PIM)
ggplot(data = df1, aes(x = bty_avg, y = score)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  geom_line(aes(y = upr), color = "purple", 
            linetype = "dashed") + 
  geom_line(aes(y = lwr), color = "purple", 
            linetype = "dashed") + 
  theme_bw() -> p1
p1


## ---------------------------------------------------------------------------------------------------------------------------
# Using the build in function
predict(score_model, newdata = data.frame(bty_avg = 7.333))
# 90% Confidence Interval for E(Y_7.333)
predict(score_model, newdata = data.frame(bty_avg = 7.333), 
        interval = "conf", level = 0.90)
# 90% Prediction Interval for Y_hat_7.333
predict(score_model, newdata = data.frame(bty_avg = 7.333), 
        interval = "pred", level = 0.90)


## ----echo = FALSE, out.height = '35%', out.width = '80%'--------------------------------------------------------------------
knitr::include_graphics("week4_7.png")


## ----echo = FALSE, out.height = '30%',out.width = '60%'---------------------------------------------------------------------
knitr::include_graphics("week4_8.png")


## ----echo = FALSE, out.height = '50%', out.width = '80%'--------------------------------------------------------------------
knitr::include_graphics("week4_9.png")


## ---------------------------------------------------------------------------------------------------------------------------
# Fit regression model:
score_model <- lm(score ~ bty_avg, data = evals_ch5)
# Get regression points:
regression_points <- get_regression_points(score_model)
head(regression_points)


## ---------------------------------------------------------------------------------------------------------------------------
# Compute sum of squared residuals
regression_points |>
  mutate(squared_residuals = residual^2) |>
  summarize(sum_of_squared_residuals = sum(squared_residuals))


## ---------------------------------------------------------------------------------------------------------------------------
# Compute sum of squared residuals
eis <- resid(score_model)
RSS <- sum(eis^2)
RSS
# or
anova(score_model)[2, 2]

