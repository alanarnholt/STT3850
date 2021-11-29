## ----setup, include=FALSE--------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = NA, fig.align = "center", warning = FALSE, message = FALSE, cache = TRUE)
library(tidyverse)
library(resampledata)
library(PASWR2)


## ----label = "ACL", fig.cap = "Random confidence intervals", fig.height = 7, fig.width = 7----
set.seed(13)
counter <- 0 # set counter to 0
mu <- 25
sigma <- 4
n <- 30
sims <- 10^4
plot(x = c(mu - 4*sigma/sqrt(n), mu + 4*sigma/sqrt(n)), y = c(1, 100), type = "n", xlab = "", ylab = "")
for (i in 1:sims){
 x <- rnorm(n, mu, sigma)
 L <- mean(x) - qnorm(0.975)*sigma/sqrt(n)
 U <- mean(x) - qnorm(0.025)*sigma/sqrt(n)
 if(L < mu && mu < U){counter <- counter + 1}
 if(i <= 100){
 segments(L, i, U, i, col = "blue")
 }
}
abline(v = mu, col = "red")
ACL <- counter/sims*100
ACL


## ----cisim-----------------------------------------------------------------------------------
library(PASWR2)
set.seed(11)
cisim(samples = 100, n = 30, parameter = 25, sigma = 4, type = "Mean")


## ----Zint------------------------------------------------------------------------------------
xs <- c(3.4, 2.9, 2.8, 5.1, 6.3, 3.9)
n <- length(xs)
SIGMA <- 2.5
alpha <- 0.10
LL <- mean(xs) - qnorm(1 - alpha/2)*SIGMA/sqrt(n)
UL <- mean(xs) + qnorm(1 - alpha/2)*SIGMA/sqrt(n)
CI <- c(LL, UL)
CI
# or use z.test() from PASWR2
z.test(x = xs, sigma.x = SIGMA, conf.level = 0.90)$conf


## ----E7.4------------------------------------------------------------------------------------
n <- ceiling((qnorm(.975)*24.6/5)^2)
n
# Using nsize from PASWR2
nsize(b = 5, sigma = 24.6, conf.level = 0.95, type = "mu")


## ----TS, fig.height=4, fig.width=7-----------------------------------------------------------
set.seed(1)
N <- 10^4
TS <- numeric(N)
n <- 16
for(i in 1:N){
  x <- rnorm(n, 25, 7)
  xbar <- mean(x)
  s <- sd(x)
  TS[i] <- (xbar - 25)/(s/sqrt(n))
}
par(mfrow=c(1, 2))
hist(TS, breaks = "Scott", freq = FALSE, col = "pink", main = "", xlab = expression(t))
qqnorm(TS, col = rgb(1, 0, 0, .1))
abline(a = 0, b = 1)
par(mfrow=c(1, 1))
# Consider qq plot for t_24
ggplot(data = data.frame(x = TS), aes(sample = x)) + 
  geom_qq(distribution = stats::qt, dparams = list(df = 24), size = 0.1, color = "blue") + 
  geom_abline(intercept = 0, slope = 1, color = "pink") + 
  theme_bw()


## ----tdfs, fig.width=7, fig.height=7---------------------------------------------------------
curve(dnorm(x, 0, 1), -4, 4, col = "black", ylab = "", xlab = "")
curve(dt(x, 1), add = TRUE, lty = 2, col = "green")
curve(dt(x, 4), add = TRUE, lty = 3, col = "pink")
curve(dt(x, 9), add = TRUE, lty = 4, col = "red")
curve(dt(x, 36), add = TRUE, lty = 5, col = "blue")
abline(h = 0, lwd=2)
legend("topright", legend = c("N(0, 1)", "t_1", "t_4", "t_9", "t_36"), lty = c(1, 2, 3, 4, 5), col =c("black", "green", "pink", "red", "blue"), lwd = 1.5)


## ---- tdfs2, fig.width = 7, fig.height = 7---------------------------------------------------
# with ggplot2
ggplot(data = data.frame(x = c(-5, 5)), aes(x = x)) + 
  theme_bw() +
  labs(x = "", y = "") +
  stat_function(fun = dt, args = list(df = 1), n = 200, color = "green", linetype = "dashed") + 
  stat_function(fun = dt, args = list(df = 4), n = 200, color = "pink", linetype = "dashed") + 
  stat_function(fun = dt, args = list(df = 9), n = 200, color = "red", linetype = "dashed") + 
  stat_function(fun = dt, args = list(df = 36), n = 200, color = "blue", linetype = "dashed") + 
  stat_function(fun = dnorm, n = 200, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 0)


## ----pt--------------------------------------------------------------------------------------
pt(2.8, 27)


## ----qt--------------------------------------------------------------------------------------
qt(.95, 27)
#
# Using function from PASWR2
tsum.test(mean.x = 110, s.x = 7.5, n.x = 28, conf.level = 0.90)$conf


## ----NCBirthsData, echo = FALSE--------------------------------------------------------------
library(resampledata)
head(NCBirths2004)
MEANS <- tapply(NCBirths2004$Weight, NCBirths2004$Gender, mean)
SD <- tapply(NCBirths2004$Weight, NCBirths2004$Gender, sd)
# OR
NCBirths2004 %>% 
  group_by(Gender) %>% 
  summarize(Mean = mean(Weight), SD = sd(Weight), n = n())


## ----QQs-------------------------------------------------------------------------------------
MEANS <- tapply(NCBirths2004$Weight, NCBirths2004$Gender, mean)
SD <- tapply(NCBirths2004$Weight, NCBirths2004$Gender, sd)
MEANS
SD
qqnorm(NCBirths2004$Weight[NCBirths2004$Gender=="Female"], main = "", col = rgb(1,0,0,.2))
qqline(NCBirths2004$Weight[NCBirths2004$Gender=="Female"], col = "blue")
# Using lattice
qqmath(~Weight|Gender,data = NCBirths2004, col = rgb(1,0,0,.1))
# Using ggplot2
ggplot(data = NCBirths2004, aes(sample = Weight)) + 
  stat_qq(color = rgb(1,0,0,.1)) + 
  stat_qq_line() +
  facet_grid(.~ Gender) + 
  theme_bw()


## ----t.test----------------------------------------------------------------------------------
girls <- subset(NCBirths2004, select = Weight, subset = Gender =="Female", drop = TRUE)
t.test(girls, conf.level = 0.99)$conf
# Or
t.test(NCBirths2004$Weight[NCBirths2004$Gender=="Female"], conf = 0.99)$conf
# Or
JG <- NCBirths2004 %>% 
  filter(Gender == "Female") 
t.test(JG$Weight, conf = 0.99)


## --------------------------------------------------------------------------------------------
B <- 10^4
bsmean <- numeric(B)
for(i in 1:B){
  bss <- sample(girls, size = length(girls), replace = TRUE)
  bsmean[i] <- mean(bss)
}
(CIperc <- quantile(bsmean, probs = c(0.025, 0.975)))
#
(CIse <- c(mean(girls) + c(-1, 1)*qt(.975, length(girls) - 1)*sd(bsmean)))
# Bootstrap t
bst <- numeric(B)
for(i in 1:B){
  bss <- sample(girls, size = length(girls), replace = TRUE)
  bst[i] <- (mean(bss) - mean(girls))/(sd(bss)/sqrt(length(girls)))
}
(Q <- quantile(bst, probs = c(0.025, 0.975)))
(CI_bst <- c(mean(girls) - Q[2]*sd(girls)/sqrt(length(girls)), mean(girls) - Q[1]*sd(girls)/sqrt(length(girls)) ))


## ----E7.7------------------------------------------------------------------------------------
set.seed(13)
tooLow <- 0       # set counter to 0
tooHigh <- 0      # set counter to 0
n <- 20           # sample size
q <- qt(0.975, n - 1)
N <- 10^5
for(i in 1:N){
  x <- rgamma(n, shape = 5, rate = 2)
  xbar <- mean(x)
  s <- sd(x)
  L <- xbar - q*s/sqrt(n)
  U <- xbar + q*s/sqrt(n)
  if(U < 5/2){tooLow <- tooLow + 1}
  if(L > 5/2){tooHigh <- tooHigh + 1}
}
TL <- tooLow/N*100
TH <- tooHigh/N*100
c(TL, TH)


## ----taccurate-------------------------------------------------------------------------------
set.seed(13)
n <- 10           # sample size
q <- qt(0.975, n - 1)
N <- 10^5
TSU <- numeric(N)
for(i in 1:N){
  x <- runif(n, 0, 1)
  xbar <- mean(x)
  s <- sd(x)
  TSU[i] <- (xbar - 0.5)/(s/sqrt(n))
}
TSE10 <- numeric(N)
for(i in 1:N){
  x <- rexp(n, 1)
  xbar <- mean(x)
  s <- sd(x)
  TSE10[i] <- (xbar - 1)/(s/sqrt(n))
}
n <- 100
TSE100 <- numeric(N)
for(i in 1:N){
  x <- rexp(n, 1)
  xbar <- mean(x)
  s <- sd(x)
  TSE100[i] <- (xbar - 1)/(s/sqrt(n))
}
#
n <- 10
qqmath(~TSU, col = "red", xlim = c(-3,3), ylim = c(-3,3), distribution = function(p){qt(p, df = n - 1)}, xlab = "Theoretical t quantiles", ylab = "Sample quantiles", main = "Uniform, n = 10", panel = function(x,...){
  panel.qqmath(x, pch = ".", ...)
  panel.abline(a = 0, b =1, ...)
})
#
qqmath(~TSE10, col = "red", xlim = c(-3,3), ylim = c(-3,3), distribution = function(p){qt(p, df = n - 1)}, xlab = "Theoretical t quantiles", ylab = "Sample quantiles", main = "Exponential, n = 10", panel = function(x,...){
  panel.qqmath(x, pch = ".", ...)
  panel.abline(a = 0, b = 1, ...)
})
#
n <- 100
qqmath(~TSE100,col = "red", xlim = c(-3,3), ylim = c(-3,3), distribution = function(p){qt(p, df = n - 1)}, xlab = "Theoretical t quantiles", ylab = "Sample quantiles", main = "Exponential, n = 100", panel = function(x,...){
  panel.qqmath(x, pch = ".", ...)
  panel.abline(a = 0, b = 1, ...)
})
#


## ----ggplots---------------------------------------------------------------------------------
DF <- data.frame(TSU, TSE10, TSE100)
p <- ggplot(data=DF, aes(sample = TSU)) + 
  stat_qq(distribution = qt, dparams = list(df = 9), pch =".", color = rgb(1,0,0,.01)) + 
  xlim(-3, 3) + 
  ylim(-3, 3) + 
  xlab("Theoretical t quantiles") + 
  ylab("Sample quantiles") + 
  ggtitle("Uniform, n = 10")
p + geom_abline(intercept = 0, slope = 1, color = "gray") + 
  theme_bw()
#
p <- ggplot(data=DF, aes(sample = TSE10)) + 
  stat_qq(distribution = qt, dparams = list(df = 9), pch =".", color = rgb(1, 0, 0, .01)) + 
  xlim(-3, 3) + 
  ylim(-3, 3) + 
  xlab("Theoretical t quantiles") + 
  ylab("Sample quantiles") + 
  ggtitle("Exponential, n = 10")
p + geom_abline(intercept = 0, slope = 1, color = "gray") + 
  theme_bw()
#
p <- ggplot(data = DF, aes(sample = TSE100)) + 
  stat_qq(distribution = qt, dparams = list(df = 99), pch =".", color = rgb(1, 0, 0, .01)) + 
  xlim(-3, 3) + 
  ylim(-3, 3) + 
  xlab("Theoretical t quantiles") + 
  ylab("Sample quantiles") + 
  ggtitle("Exponential, n = 100")
p + geom_abline(intercept = 0, slope = 1, color = "gray") +
  theme_bw()


## ----label = "CALC"--------------------------------------------------------------------------
# Something new
library(PASWR2)  
library(ggplot2)


## ----NORMALITY-------------------------------------------------------------------------------
ggplot(data = CALCULUS, aes(sample = score, color = calculus)) + 
  stat_qq() + 
  stat_qq_line() +
  theme_bw()


## ----CalcTtest-------------------------------------------------------------------------------
t.test(score~calculus, data = CALCULUS, conf.level = 0.90)
t.test(score~calculus, data = CALCULUS, conf.level = 0.90)$conf.int


## --------------------------------------------------------------------------------------------
library(infer)
CI <- CALCULUS %>% 
  specify(score ~ calculus) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "diff in means", order = c("No", "Yes"))
get_ci(CI, level = 0.95)
# Using a for loop
NoCalc <- CALCULUS$score[CALCULUS$calculus == "No"]
# Or using 
YesCalc <- subset(CALCULUS, select = score, calculus == "Yes", drop = TRUE)
YesCalc
#
B <- 10^3
diffmean <- numeric(B)
for(i in 1:B){
  bsno <- sample(NoCalc, size = length(NoCalc), replace = TRUE)
  bsyes <- sample(YesCalc, size = length(YesCalc), replace = TRUE)
  diffmean[i] <- mean(bsno) - mean(bsyes)
}
(CIperc <- quantile(diffmean, probs = c(0.025, 0.975)))
# Bootstrap t next
B <- 10^3
bst <- numeric(B)
for(i in 1:B){
  bsno <- sample(NoCalc, size = length(NoCalc), replace = TRUE)
  bsyes <- sample(YesCalc, size = length(YesCalc), replace = TRUE)
  bst[i] <- ( (mean(bsno) - mean(bsyes)) - (mean(NoCalc) - mean(YesCalc)) ) / (sqrt(var(bsno)/length(NoCalc) + var(bsyes)/length(YesCalc)))
}
(Q <- quantile(bst, probs = c(0.025, 0.975)))
(CI_bst <- c((mean(NoCalc) - mean(YesCalc)) - Q[2]*(sqrt(var(NoCalc)/length(NoCalc) + var(YesCalc)/length(YesCalc))), 
(mean(NoCalc) - mean(YesCalc)) - Q[1]*(sqrt(var(NoCalc)/length(NoCalc) + var(YesCalc)/length(YesCalc)))))


## --------------------------------------------------------------------------------------------
library(boot)
mean2.boot <- function(data, i){
  d <- data[i, ]
  M <- tapply(d$score, d$calculus, mean)
  V <- tapply(d$score, d$calculus, var)/tapply(d$score, d$calculus, length)
  return(c(M[1] - M[2], V[1] + V[2]))
}
set.seed(1)
boot.out <- boot(CALCULUS, mean2.boot, R=10^4, strata = CALCULUS[ ,2])
boot.ci(boot.out, conf = 0.95, type = c("perc", "stud", "norm", "basic", "bca"))


## ----Texas-----------------------------------------------------------------------------------
Texas <- TXBirths2004
ggplot(data = Texas, aes(x = Weight)) + 
  geom_histogram(fill = "blue", color = "black") + 
  facet_grid(Gender~.) + 
  theme_bw()
ggplot(data = Texas, aes(sample = Weight)) + 
  stat_qq(color = rgb(0, 0, 1, 0.15)) + 
  facet_grid(Gender~.) + 
  theme_bw()


## ----Texas2----------------------------------------------------------------------------------
t.test(Texas$Weight ~ Texas$Gender)
str(Texas)
Texas <- within(data = Texas, expr={Gender <- factor(Gender, levels =c("Male", "Female"))})
str(Texas)
t.test(Texas$Weight ~ Texas$Gender)


## ----prop------------------------------------------------------------------------------------
prop.test(x = 499, n = 846, conf.level = 0.90, correct = FALSE)
prop.test(x = 499, n = 846, conf.level = 0.90, correct = FALSE)$conf


## ----agresti---------------------------------------------------------------------------------
xtilde <- 132
ntilde <- 214
ptilde <- xtilde/ntilde
ptilde + c(-1, 1)*qnorm(.975)*sqrt(ptilde*(1 - ptilde)/ntilde)
# Compare to
prop.test(x = 130, n = 210, correct = FALSE)


## ----maxp------------------------------------------------------------------------------------
ptilde <- seq(0, 1, length= 1000)
fptilde <- sqrt(ptilde*(1 - ptilde))
plot(ptilde, fptilde, type = "l", ylab = "", xlab =expression(tilde(p)))


## ----samplesize------------------------------------------------------------------------------
ntilde <- (1.96*(0.5)/0.04)^2
n <- ntilde - 4
n <- ceiling(n)
n


## ----coverage--------------------------------------------------------------------------------
alpha <- 0.05
n <- 30   # number of trials
x <- 0:n  
sp <- x/n # sample proportion
m.err <- qnorm(1 - alpha/2)*sqrt(sp*(1 - sp)/n)
lcl <- sp - m.err
ucl <- sp + m.err
pp <- 0.8   # pp = P(Success)
prob <- dbinom(x, n, pp) # P(X=x)
cover <- (pp >= lcl) & (pp <= ucl)  # vector of 0s and 1s
RES <- round(cbind(x, sp, lcl, ucl, prob, cover), 4)
RES[18:31, ]
sum(dbinom(x[cover], n, pp))  # total coverage prob at pp


## ----coverage79------------------------------------------------------------------------------
alpha <- 0.05
n <- 30   # number of trials
x <- 0:n  
sp <- x/n # sample proportion
m.err <- qnorm(1 - alpha/2)*sqrt(sp*(1 - sp)/n)
lcl <- sp - m.err
ucl <- sp + m.err
pp <- 0.79   # pp = P(Success)
prob <- dbinom(x, n, pp)
cover <- (pp >= lcl) & (pp <= ucl)  # vector of 0s and 1s
RES <- round(cbind(x, sp, lcl, ucl, prob, cover), 4)
RES[18:31, ]
sum(dbinom(x[cover], n, pp))  # total coverage prob at pp


## ----coveragePLOT, fig.width=7, fig.height=7-------------------------------------------------
opar <- par(no.readonly = TRUE)
par(mfrow=c(2, 2))
for(alpha in c(0.01, 0.02, 0.05, 0.10)){
n <- 30     # number of trials
CL <- 1 - alpha
x <- 0:n 
adj <- 0    #(2 for Agresti-Coull)
k <- qnorm(1 - alpha/2)
sp <- (x + adj)/(n + 2*adj)
m.err <- k * sqrt(sp*(1 - sp)/(n + 2*adj))
lcl <- sp - m.err
ucl <- sp + m.err
m <- 2000 # number of values of pp
pp <- seq(1/n, 1 - 1/n, length = m)
p.cov <- numeric(m)
for(i in 1:m){
  cover <- (pp[i] >= lcl) & (pp[i] <= ucl)  # vector of 0s and 1s
  p.rel <- dbinom(x[cover], n, pp[i])
  p.cov[i] <- sum(p.rel)
}
plot(pp, p.cov, type = "l", ylim =c(0.60, 1.1), main = paste("n = ", n), xlab = "p", ylab = "Coverage Probability")
lines(c(1/n, 1- 1/n), c(1 - alpha, 1- alpha), col = "red", lty = "dashed")
      text(0.5, CL + 0.05, paste("Targeted Confidence Level =", CL))
}
par(opar)


## ----WilsonCover, fig.width=7, fig.height=7-https://mathr.math.appstate.edu/s/ecb2da37a34ff24bcf32f/graphics/plot_zoom_png?width=596&height=476-------------------------------------------------
opar <- par(no.readonly = TRUE)
par(mfrow=c(2, 2))
for(alpha in c(0.01, 0.02, 0.05, 0.10)){
n <- 30     # number of trials
CL <- 1 - alpha
x <- 0:n 
z <- qnorm(1 - alpha/2)
sp <- x/n
sptilda <- (x + z^2/2)/(n + z^2)
m.err <- (z/(n + z^2))*sqrt(n*sp*(1 - sp) + z^2/4)
lcl <- sptilda - m.err
ucl <- sptilda + m.err
m <- 2000 # number of values of pp
pp <- seq(1/n, 1 - 1/n, length = m)
p.cov <- numeric(m)
for(i in 1:m){
  cover <- (pp[i] >= lcl) & (pp[i] <= ucl)  # vector of 0s and 1s
  p.rel <- dbinom(x[cover], n, pp[i])
  p.cov[i] <- sum(p.rel)
}
plot(pp, p.cov, type = "l", ylim =c(0.60, 1.1), main = paste("n = ", n), xlab = "p", ylab = "Coverage Probability")
lines(c(1/n, 1- 1/n), c(1 - alpha, 1- alpha), col = "red", lty = "dashed")
      text(0.5, CL + 0.05, paste("Targeted Confidence Level =", CL))
}
par(opar)


## ----ACCover, fig.width=7, fig.height=7------------------------------------------------------
opar <- par(no.readonly = TRUE)
par(mfrow=c(2, 2))
for(alpha in c(0.01, 0.02, 0.05, 0.10)){
n <- 30     # number of trials
CL <- 1 - alpha
x <- 0:n 
adj <- 2  # 0 for large sample 2 for Agresti Coull
z <- qnorm(1 - alpha/2)
sp <- (x + adj)/(n + 2*adj)
m.err <- z*sqrt(sp*(1 - sp)/(n + 2*adj))
lcl <- sp - m.err
ucl <- sp + m.err
m <- 2000 # number of values of pp
pp <- seq(1/n, 1 - 1/n, length = m)
p.cov <- numeric(m)
for(i in 1:m){
  cover <- (pp[i] >= lcl) & (pp[i] <= ucl)  # vector of 0s and 1s
  p.rel <- dbinom(x[cover], n, pp[i])
  p.cov[i] <- sum(p.rel)
}
plot(pp, p.cov, type = "l", ylim =c(0.60, 1.1), main = paste("n = ", n), xlab = "p", ylab = "Coverage Probability")
lines(c(1/n, 1- 1/n), c(1 - alpha, 1- alpha), col = "red", lty = "dashed")
      text(0.5, CL + 0.05, paste("Targeted Confidence Level =", CL))
}
par(opar)


## ----BangladeshA-----------------------------------------------------------------------------
head(Bangladesh)
ggplot(data = Bangladesh, aes(x = Arsenic)) + 
  geom_histogram(fill = "blue", color = "black",
                 binwidth = 100) + 
  theme_bw()


## ----BOOTarsenic, fig.width=7, fig.height=7--------------------------------------------------
Arsenic <- subset(Bangladesh, select = Arsenic, drop = T)
xbar <- mean(Arsenic)
S <- sd(Arsenic)
N <- 10^5
n <- length(Arsenic)
Tstar <- numeric(N)
Sstar <- numeric(N)
Xbarstar <- numeric(N)
set.seed(13)
for (i in 1:N)
{
  x <-sample(Arsenic, size = n, replace = T)
  Xbarstar[i] <- mean(x)
  Sstar[i] <- sd(x)
}
Tstar <- (Xbarstar - xbar)/(Sstar / sqrt(n))
CIt <- quantile(Tstar, c(0.025, 0.975))
names(CIt) <- NULL
CIt
opar <- par(no.readonly = TRUE)
par(mfrow= c(2, 2))
plot(Xbarstar, Sstar, ylab = "S*", xlab = substitute(paste(bar(X),"*")), col = rgb(1,0,0,0.01))
qqnorm(Tstar, col = rgb(1,0,0,0.01))
qqline(Tstar)
hist(Tstar, xlab = "T*", main = "Bootstrap distribution of T*", col = "red", breaks = "Scott")
hist(Xbarstar, xlab = substitute(paste(bar(X),"*")), main = substitute(paste("Bootstrap Distribution of ", bar(X),"*")), col = "red", breaks = "Scott")
par(opar)


## ----BootCI----------------------------------------------------------------------------------
LL <- xbar - CIt[2]*S/sqrt(n)
UL <- xbar - CIt[1]*S/sqrt(n)
c(LL, UL)


## ----packageboot, fig.height=3.5, fig.width=7------------------------------------------------
require(boot)
mean.boot <- function(data, i){
  d <- data[i]
  M <- mean(d)
  V <- var(d)/length(i)
  return(c(M, V))
}
boot.out <- boot(Arsenic, mean.boot, R=10^5)
boot.ci(boot.out, conf = 0.95, type = c("perc", "stud"))
plot(boot.out)
hist(boot.out$t[,1], col = "pink", breaks = "Scott", main = "", xlab = substitute(paste(bar(X),"*")), freq= FALSE)
lines(density(boot.out$t[,1]), lwd = 2)
hist((boot.out$t[,1] - boot.out$t0[1])/(boot.out$t[,2])^.5, col = "pink", breaks = "Scott", main = "", xlab ="T*", freq= FALSE)
lines(density((boot.out$t[,1] - boot.out$t0[1])/(boot.out$t[,2])^.5), lwd = 2)


## ----verizon---------------------------------------------------------------------------------
Time.ILEC <- subset(Verizon, select=Time, Group == "ILEC", drop=TRUE)
Time.CLEC <- subset(Verizon, select=Time, Group == "CLEC", drop=TRUE)
thetahat <- mean(Time.ILEC) - mean(Time.CLEC)
nx <- length(Time.ILEC)  #nx=1664
ny <- length(Time.CLEC)  #ny=23
SE <- sqrt(var(Time.ILEC)/nx + var(Time.CLEC)/ny)
N <- 10^4
Tstar <- numeric(N)
DM <- numeric(N)
set.seed(1)
for(i in 1:N)
{
  bootx <- sample(Time.ILEC, nx, replace=TRUE)
  booty <- sample(Time.CLEC, ny, replace=TRUE)
  Tstar[i] <- (mean(bootx) - mean(booty) - thetahat) /
    sqrt(var(bootx)/nx + var(booty)/ny)
  DM[i] <- mean(bootx) - mean(booty)
}
quantile(Tstar, c(.975, .025))
CItboot <- thetahat - quantile(Tstar, c(.975, .025)) * SE
names(CItboot) <- NULL
CItboot
CIperct <- quantile(DM, c(0.025, 0.975))
CIperct
t.test(Time.ILEC, Time.CLEC)$conf


## ----packageboot2m, fig.show='asis'----------------------------------------------------------
require(boot)
mean2.boot <- function(data, i){
  d <- data[i, ]
  M <- tapply(d$Time, d$Group, mean)
  V <- tapply(d$Time, d$Group, var)/tapply(d$Time, d$Group, length)
  return(c(M[2] - M[1], V[2] + V[1]))
}
set.seed(1)
boot.out <- boot(Verizon, mean2.boot, R=10^4, strata = Verizon[ ,2])
boot.ci(boot.out, conf = 0.95, type = c("perc", "stud"))
plot(boot.out)
hist(boot.out$t[,1], col = "pink", breaks = "Scott", main = "", freq= FALSE, xlab = substitute(paste(bar(x)[1],"* - ", bar(x)[2],"*")))
lines(density(boot.out$t[,1]), lwd = 2)
qqnorm(boot.out$t[,1], col =rgb(1,0,0,.05))
qqline(boot.out$t[,1])
# ggplot2 Now
require(ggplot2)
BO <- as.data.frame(boot.out$t)
ggplot(data = BO, aes(x = V1, y = ..density..)) + 
  geom_histogram(fill = "pink") + 
  xlab(substitute(paste(bar(x)[1],"* - ", bar(x)[2],"*"))) + 
  geom_density(size = 1.5, color = "red") + 
  theme_bw()
p <- ggplot(data = BO, aes(sample = V1)) + 
  stat_qq(pch = ".", color = rgb(1, 0, 0, 0.1)) +
  theme_bw()
p
hist((boot.out$t[,1] - boot.out$t0[1])/(boot.out$t[,2])^.5, col = "pink", breaks = "Scott", main = "", xlab ="T*", freq= FALSE)
lines(density((boot.out$t[,1] - boot.out$t0[1])/(boot.out$t[,2])^.5), lwd = 2)
qqnorm((boot.out$t[,1] - boot.out$t0[1])/(boot.out$t[,2])^.5, col = rgb(1,0,0,.05))
qqline((boot.out$t[,1] - boot.out$t0[1])/(boot.out$t[,2])^.5)


## ----skew------------------------------------------------------------------------------------
require(e1071)
skewness(Verizon$Time)

