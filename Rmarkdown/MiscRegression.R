
set.seed(123)
library(knitr)
library(tidyverse)
knitr::opts_chunk$set(comment = NA,  fig.align = 'center', fig.height = 5, fig.width = 5, prompt = FALSE, highlight = TRUE, tidy = FALSE, warning = FALSE, message = FALSE, tidy.opts=list(blank = TRUE, width.cutoff= 75, cache = TRUE))



site <- "http://statlearning.com/s/Advertising.csv"
AD <- read.csv(site)
head(AD)
dim(AD)
library(DT)
datatable(AD[, -1], rownames = FALSE,
          caption = 'Table 1: This is a simple caption for the table.') 


#| label: "fig-base1"
#| fig-cap: "Base R scatterplot of `sales` versus `TV`"
plot(sales ~ TV, data = AD, col = "red", pch = 19)
mod1 <- lm(sales ~ TV, data = AD)
abline(mod1, col = "blue")


#| label: "fig-youchange"
#| fig-cap: "You should change this caption"
par(mfrow = c(1, 3))
plot(sales ~ TV, data = AD, col = "red", pch = 19)
mod1 <- lm(sales ~ TV, data = AD)
abline(mod1, col = "blue")
plot(sales ~ radio, data = AD, col = "red", pch = 19)
mod2 <- lm(sales ~ radio, data = AD)
abline(mod2, col = "blue")
plot(sales ~ newspaper, data = AD, col = "red", pch = 19)
mod3 <- lm(sales ~ newspaper, data = AD)
abline(mod3, col = "blue")
par(mfrow=c(1, 1))


#| label: "fig-ggp"
#| fig-cap: "`ggplot` graph with three superimposed lines"
library(ggplot2)
library(MASS)
p <- ggplot(data = AD, aes(x = TV, y = sales)) +
  geom_point(color = "lightblue") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) + 
  geom_smooth(method = "rlm", color = "purple", se = FALSE) +
  theme_bw()
p


#| label: "fig-gm"
#| fig-cap: "Using `grid.arrange()` with `ggplot`"
library(gridExtra)
p1 <- ggplot(data = AD, aes(x = TV, y = sales)) +
        geom_point(color = "lightblue") + 
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        theme_bw()
p2 <- ggplot(data = AD, aes(x = radio, y = sales)) +
        geom_point(color = "lightblue") +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        theme_bw()
p3 <- ggplot(data = AD, aes(x = newspaper, y = sales)) +
        geom_point(color = "lightblue") +
        geom_smooth(method = "lm", se = FALSE, color = "blue") + 
        theme_bw()
grid.arrange(p1, p2, p3, ncol = 3)


#| fig-cap: "Using `patchwork` with `ggplot`"
library(patchwork)
p1 + p2 + p3



library(ggvis)
AD %>% 
  ggvis(x = ~TV, y = ~sales) %>% 
  layer_points() %>% 
  layer_model_predictions(model = "lm", se = FALSE) %>% 
  layer_model_predictions(model = "MASS::rlm", se = FALSE, stroke := "blue") %>%
  layer_smooths(stroke:="red", se = FALSE)



library(plotly)
p11 <- ggplotly(p)
p11



library(car)
scatterplotMatrix(~ sales + TV + radio + newspaper, data = AD)



mod1 <- lm(sales ~ TV, data = AD)
summary(mod1)



eis <- resid(mod1)
RSS <- sum(eis^2)
RSS
RSE <- sqrt(RSS/(dim(AD)[1]-2))
RSE
# Or
summary(mod1)$sigma
# Or
library(broom)
NDF <- augment(mod1)
sum(NDF$.resid^2)
RSE <- sqrt(sum(NDF$.resid^2)/df.residual(mod1))
RSE



library(moderndive)
get_regression_table(mod1)
MDDF <- get_regression_points(mod1)
MDDF
library(dplyr)
MDDF %>% 
  summarize(RSS = sum(residual^2))



y <- AD$sales
x <- AD$TV
b1 <- sum( (x - mean(x))*(y - mean(y)) ) / sum((x - mean(x))^2)
b0 <- mean(y) - b1*mean(x)
c(b0, b1)
# Or using
coef(mod1)
summary(mod1)
XTXI <- summary(mod1)$cov.unscaled
MSE <- summary(mod1)$sigma^2
var.cov.b <- MSE*XTXI
var.cov.b
seb0 <- sqrt(var.cov.b[1, 1])
seb1 <- sqrt(var.cov.b[2, 2])
c(seb0, seb1)
coef(summary(mod1))
coef(summary(mod1))[1, 2]
coef(summary(mod1))[2, 2]
tb0 <- b0/seb0
tb1 <- b1/seb1
c(tb0, tb1)
pvalues <- c(pt(tb0, 198, lower = FALSE)*2, pt(tb1, 198, lower = FALSE)*2)
pvalues
coef(summary(mod1))
TSS <- sum((y - mean(y))^2)
c(RSS, TSS)
R2 <- (TSS - RSS)/TSS
R2
# Or
summary(mod1)$r.squared



alpha <- 0.10
ct <- qt(1 - alpha/2, df.residual(mod1))
ct
b1 + c(-1, 1)*ct*seb1
# Or
confint(mod1, parm = "TV", level = 0.90)
confint(mod1)
# Or 
library(moderndive)
get_regression_table(mod1)



A <- matrix(c(2, -3, -2, 1, -1, 1, -1, 2, 2), nrow = 3)
b <- matrix(c(8, -11, -3), nrow = 3)
x <- solve(A)%*%b
x
# Or
solve(A, b)



A <- matrix(c(2, 9, 4, 5), nrow = 2)
A
t(A)          # Transpose of A
t(A)%*%A      # A'A
solve(A)%*%A  # I_2
zapsmall(solve(A)%*%A)  # What you expect I_2



X <- model.matrix(mod1)
XTX <- t(X)%*%X
dim(XTX)
XTXI <- solve(XTX)
XTXI
# But it is best to compute this quantity using
summary(mod1)$cov.unscaled
betahat <- XTXI%*%t(X)%*%y
betahat
coef(mod1)
XTXI <- summary(mod1)$cov.unscaled
MSE <- summary(mod1)$sigma^2
var_cov_b <- MSE*XTXI
var_cov_b



library(PASWR2)
mod.lm <- lm(gpa ~ sat, data = GRADES)
summary(mod.lm)
betahat <- coef(mod.lm)
betahat
knitr::kable(tidy(mod.lm))
#
Xh <- matrix(c(1, 1300), nrow = 1)
Yhath <- Xh%*%betahat
Yhath
predict(mod.lm, newdata = data.frame(sat = 1300))
# Linear Algebra First
anova(mod.lm)
MSE <- anova(mod.lm)[2, 3]
MSE
XTXI <- summary(mod.lm)$cov.unscaled
XTXI
var_cov_b <- MSE*XTXI
var_cov_b
s2yhath <- Xh %*% var_cov_b %*% t(Xh)
s2yhath
syhath <- sqrt(s2yhath)
syhath
crit_t <- qt(0.95, df.residual(mod.lm))
crit_t
CI_EYh <- c(Yhath) + c(-1, 1)*c(crit_t*syhath)
CI_EYh
# Using the build in function
predict(mod.lm, newdata = data.frame(sat = 1300), interval = "conf", level = 0.90)



mod2 <- lm(sales ~ TV + radio, data = AD)
summary(mod2)


#| label: "fig-mucho"
#| fig-cap: "3-D residuals and fitted plane"
library(scatterplot3d)
s3d <- scatterplot3d(x = AD$TV, y = AD$radio, 
              z = AD$sales, xlab = "TV", 
              ylab = "Radio", zlab = "Sales",
              box = TRUE, pch = 20, color = "white",
              cex.symbols = 0.75, angle = 60, grid = FALSE)
s3d$plane3d(mod2 <- lm(sales ~ TV + radio, data = AD), 
            lty = "dotted", lty.box = "solid")
orig <- s3d$xyz.convert(x = AD$TV, y = AD$radio, 
                        z = AD$sales)
plane <- s3d$xyz.convert(x = AD$TV, y = AD$radio,  fitted(mod2))
i.negpos <- 1 + (resid(mod2) > 0)
segments(orig$x, orig$y, plane$x, plane$y,
         col = c("darkblue", "lightblue3")[i.negpos])
s3d$points3d(x = AD$TV, y = AD$radio, 
             z = AD$sales,
             col = c("darkblue", "lightblue3")[i.negpos],
             pch = 20)



library(plotly)
# draw the 3D scatterplot
p <- plot_ly(data = AD, z = ~sales, x = ~TV, y = ~radio, opacity = 0.5) %>% 
  add_markers
p



x <- seq(0, 300, length = 70)
y <- seq(0, 50, length = 70)
plane <- outer(x, y, function(a, b){summary(mod2)$coef[1, 1] + summary(mod2)$coef[2, 1]*a + summary(mod2)$coef[3, 1]*b})
# draw the plane
p %>% 
  add_surface(x = ~x, y = ~y, z = ~plane, showscale = FALSE)



mod3 <- lm(sales ~ TV + radio + newspaper, data = AD)
summary(mod3)



anova(mod3)
SSR <- sum(anova(mod3)[1:3, 2])
MSR <- SSR/3
SSE <- anova(mod3)[4, 2]
MSE <- SSE/(200-3-1)
Fobs <- MSR/MSE
Fobs
pvalue <- pf(Fobs, 3, 196, lower = FALSE)
pvalue
# Or
summary(mod3)
summary(mod3)$fstatistic



summary(mod3)
anova(mod1, mod3)



mod.fs <- lm(sales ~ 1, data = AD)
SCOPE <- (~ TV + radio + newspaper)
add1(mod.fs, scope = SCOPE, test = "F")
mod.fs <- update(mod.fs, .~. + TV)
add1(mod.fs, scope = SCOPE, test = "F")
mod.fs <- update(mod.fs, .~. + radio)
add1(mod.fs, scope = SCOPE, test = "F")
summary(mod.fs)



stepAIC(lm(sales ~ 1, data = AD), scope = (~TV + radio + newspaper), direction = "forward", test = "F")
# Or
null <- lm(sales ~ 1, data = AD)
full <- lm(sales ~ ., data = AD)
stepAIC(null, scope = list(lower = null, upper = full), direction = "forward", test = "F")



mod.be <- lm(sales ~ TV + radio + newspaper, data = AD)
drop1(mod.be, test = "F")
mod.be <- update(mod.be, .~. - newspaper)
drop1(mod.be, test = "F")
summary(mod.be)



stepAIC(lm(sales ~ TV + radio + newspaper, data = AD), scope = (~TV + radio + newspaper), direction = "backward", test = "F")
# Or
stepAIC(full, scope = list(lower = null, upper = full), direction = "backward", test = "F")



residualPlots(mod2)
qqPlot(mod2)
influenceIndexPlot(mod2)



predict(mod.be, newdata = data.frame(TV = 100, radio = 20), interval = "conf")



predict(mod.be, newdata = data.frame(TV = 100, radio = 20), interval = "pred")



nam1 <- lm(sales ~ TV*radio, data = AD)
# Same as 
nam2 <- lm(sales ~ TV + radio + TV:radio, data = AD)
summary(nam1)
summary(nam2)



Credit <- read.csv("http://statlearning.com/s/Credit.csv")
datatable(Credit[, -1], rownames = FALSE)



modP <- lm(Balance ~ Income*Student, data = Credit)
summary(modP)



library(ISLR)
data(Credit)
modS <- lm(Balance ~ Gender, data = Credit)
summary(modS)
coef(modS)
tapply(Credit$Balance, Credit$Gender, mean)
library(ggplot2)
ggplot(data = Credit, aes(x = Gender, y = Balance)) + 
  geom_point() + 
  theme_bw() + 
  geom_hline(yintercept = coef(modS)[1] + coef(modS)[2], color = "purple") + 
  geom_hline(yintercept = coef(modS)[1], color = "green")



Credit$Utilization <- Credit$Balance / (Credit$Income*100)
tapply(Credit$Utilization, Credit$Gender, mean)
# Tidyverse approach
Credit %>%
  mutate(Ratio = Balance / (Income*100) ) %>%
  group_by(Gender) %>%
  summarize(mean(Ratio))



modU <- lm(Utilization ~ Gender, data = Credit)
summary(modU)
coef(modU)
ggplot(data = Credit, aes(x = Gender, y = Utilization)) + 
  geom_point() + 
  theme_bw() + 
  geom_hline(yintercept = coef(modU)[1] + coef(modU)[2], color = "purple") + 
  geom_hline(yintercept = coef(modU)[1], color = "green")



modS1 <- lm(Balance ~ Limit + Student, data = Credit)
summary(modS1)
coef(modS1)
# Interaction --- Non-additive Model
modS2 <- lm(Balance ~ Limit*Student, data = Credit)
summary(modS2)


#| label: "fig-GGDWDYE"
#| fig-cap: " Balance versus Limit"
ggplot(data = Credit, aes(x = Limit, y = Balance, color = Student)) + 
  geom_point() + 
  stat_smooth(method = "lm") + 
  theme_bw()


#| label: "fig-CG"
#| fig-cap: "Figure this one out!"
S2M <- lm(Balance ~ Limit + Student, data = Credit)
#
ggplot(data = Credit, aes(x = Limit, y = Balance, color = Student)) +
  geom_point() + 
  theme_bw() + 
  geom_abline(intercept = coef(S2M)[1], slope = coef(S2M)[2], color = "red") + 
  geom_abline(intercept = coef(S2M)[1] + coef(S2M)[3], slope = coef(S2M)[2], color = "blue") + 
  scale_color_manual(values = c("red", "blue"))



modQ3 <- lm(Balance ~ Limit + Ethnicity, data = Credit)
summary(modQ3)
coef(modQ3)
modRM <- lm(Balance ~ Limit, data = Credit)
anova(modRM, modQ3)



AfAmer <- lm(Balance ~ Limit, data = subset(Credit, Ethnicity == "African American"))
AsAmer <- lm(Balance ~ Limit, data = subset(Credit, Ethnicity == "Asian"))
CaAmer <- lm(Balance ~ Limit, data = subset(Credit, Ethnicity == "Caucasian"))
rbind(coef(AfAmer), coef(AsAmer), coef(CaAmer))
ggplot(data = Credit, aes(x = Limit, y = Balance, color = Ethnicity)) +
  geom_point() + 
  theme_bw() +
  stat_smooth(method = "lm", se = FALSE)



ggplot(data = Credit, aes(x = Limit, y = Balance)) +
  geom_point(aes(color = Ethnicity)) + 
  theme_bw() +
  stat_smooth(method = "lm")



scatterplotMatrix(~ Balance + Income + Limit + Rating + Cards + Age + Education + Gender + Student + Married + Ethnicity,  data = Credit)
null <- lm(Balance ~ 1, data = Credit)
full <- lm(Balance ~ ., data = Credit)
modC <- stepAIC(full, scope = list(lower = null, upper = full), direction = "backward", test = "F")
modC
modD <- stepAIC(null, scope = list(lower = null, upper = full), direction = "forward", test = "F")
modD
# Predict
predict(modC, newdata = data.frame(Income = 80, Limit = 5000, Cards = 3, Age = 52, Student = "No", Rating = 800, Utilization = 0.10), interval = "pred")



residualPlots(modC)
qqPlot(modC)
influenceIndexPlot(modC)


#| label: "fig-NLR"
#| fig-cap: "Showing non-linear relationships"
library(ISLR)
car1 <- lm(mpg ~ horsepower, data = Auto)
car2 <- lm(mpg ~ poly(horsepower, 2), data = Auto)
car5 <- lm(mpg ~ poly(horsepower, 5), data = Auto)
xs <- seq(min(Auto$horsepower), max(Auto$horsepower), length = 500)
y1 <- predict(car1, newdata = data.frame(horsepower = xs))
y2 <- predict(car2, newdata = data.frame(horsepower = xs))
y5 <- predict(car5, newdata = data.frame(horsepower = xs))
DF <- data.frame(x = xs, y1 = y1, y2 = y2, y5 = y5)
ggplot(data = Auto, aes(x = horsepower, y = mpg)) + 
  geom_point() + 
  theme_bw() + 
  geom_line(data = DF, aes(x = x, y = y1), color = "red") + 
  geom_line(data = DF, aes(x = x, y = y2), color = "blue") + 
  geom_line(data = DF, aes(x = x, y = y5), color = "green")



ggplot(data = Auto, aes(x = horsepower, y = mpg)) + 
  geom_point(color = "lightblue") + 
  theme_bw() + 
  stat_smooth(method = "lm", data = Auto, color = "red", se = FALSE) + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), data = Auto, color = "blue", se = FALSE) + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 5), data = Auto, color = "green", se = FALSE) 



newC <- update(modC, .~. - Limit - Income - Rating + poly(Income, 2) + poly(Limit, 4))
summary(newC)
residualPlots(newC)
qqPlot(newC)
influenceIndexPlot(newC)



modC
R2inc <- summary(lm(Income ~ Limit + Rating + Cards + Age + Student + Utilization, data = Credit))$r.squared
R2inc
VIFinc <- 1/(1 - R2inc)
VIFinc
R2lim <- summary(lm(Limit ~ Income + Rating + Cards + Age + Student + Utilization, data = Credit))$r.squared
R2lim
VIFlim <- 1/(1 - R2lim)
VIFlim



car::vif(modC)

