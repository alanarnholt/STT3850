## ---- MEDskip, echo = FALSE-------------------------------------------------------------
library(knitr)
knit_hooks$set(document = function(x){
gsub("\\begin{tabular}", "\\medskip{}\\begin{tabular}", x, fixed = TRUE)
})


## ----setup, include = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = NA, warning = FALSE, message = FALSE, fig.align = 'center')


## ---------------------------------------------------------------------------------------
library(tidyverse)
library(moderndive)
library(infer)
library(resampledata)
library(PASWR2)


## ---- out.height = '50%', out.width = '70%'---------------------------------------------
ggplot(data = CALCULUS, aes(sample = score, color = calculus)) + 
  stat_qq() + 
  stat_qq_line() +
  theme_bw()


## ---------------------------------------------------------------------------------------
CALCULUS %>% 
  group_by(calculus) %>% 
  summarize(Mean = mean(score), n = n(), SD = sd(score))


## ---------------------------------------------------------------------------------------
t.test(score ~ calculus, data = CALCULUS, conf.level = 0.90)
t.test(score ~ calculus, data = CALCULUS, conf.level = 0.90)$conf.int -> TTCI
TTCI


## ---------------------------------------------------------------------------------------
library(infer)
CI <- CALCULUS %>% 
  specify(score ~ calculus) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "diff in means", order = c("No", "Yes"))
get_ci(CI, level = 0.90)
# Compare to Theoretical T CI below
TTCI


## ---- out.height = '50%',out.width = '70%'----------------------------------------------
Texas <- TXBirths2004
ggplot(data = Texas, aes(x = Weight)) + 
  geom_histogram(fill = "blue", color = "black") + 
  facet_grid(rows = vars(Gender)) + 
  theme_bw()


## ---- out.height = '60%',out.width = '80%'----------------------------------------------
ggplot(data = Texas, aes(sample = Weight)) + 
  stat_qq(color = rgb(0, 0, 1, 0.15)) + 
  facet_grid(rows = vars(Gender)) + 
  theme_bw()


## ---------------------------------------------------------------------------------------
t.test(Weight ~ Gender, data = Texas)
Texas$Gender <- factor(Texas$Gender, levels = c("Male", "Female"))
t.test(Weight ~ Gender, data = Texas)


## ---------------------------------------------------------------------------------------
x <- 0.59*846
n <- 846
phat <- x/n
phat + c(-1, 1)*qnorm(.975)*sqrt(phat*(1 - phat)/n)
library(binom)
binom.confint(x = .59*846, n = 846, methods = "asymptotic")


## ---------------------------------------------------------------------------------------
xtilde <- 0.59*846 + 2
ntilde <- 846 + 4
ptilde <- xtilde/ntilde
ptilde + c(-1, 1)*qnorm(.975)*sqrt(ptilde*(1 - ptilde)/ntilde)
# Or
binom.confint(x = .59*846, n = 846, methods = "ac")


## ---------------------------------------------------------------------------------------
prop.test(x = 499.14, n = 846, conf.level = 0.95, correct = FALSE)
# Or
binom.confint(x = .59*846, n = 846, methods = "wilson")


## ---- out.height = '40%', out.width = '60%'---------------------------------------------
ptilde <- seq(0, 1, length = 1000)
fptilde <- sqrt(ptilde*(1 - ptilde))
plot(ptilde, fptilde, type = "l", ylab = "", xlab =expression(tilde(p)))


## ---------------------------------------------------------------------------------------
ntilde <- (1.96*(0.5)/0.04)^2
n <- ntilde - 4
n <- ceiling(n)
n


## ---------------------------------------------------------------------------------------
alpha <- 0.05
n <- 30   # number of trials
x <- 0:n  
sp <- x/n # sample proportion
m.err <- qnorm(1 - alpha/2)*sqrt(sp*(1 - sp)/n)
lcl <- sp - m.err
ucl <- sp + m.err
pp <- 0.8   # pp = P(Success)
prob <- dbinom(x, n, pp)
cover <- (pp >= lcl) & (pp <= ucl)  # vector of 0s and 1s
RES <- round(cbind(x, sp, lcl, ucl, prob, cover), 4)
RES[19:30, ]
sum(dbinom(x[cover], n, pp))  # total coverage prob at pp


## ---------------------------------------------------------------------------------------
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
#RES[18:31, ]
sum(dbinom(x[cover], n, pp))  # total coverage prob at pp


## ---- eval=FALSE------------------------------------------------------------------------
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
plot(pp, p.cov, type = "l", ylim =c(0.60, 1.1), main = paste("n = ", n), 
     xlab = "p", ylab = "Coverage Probability")
lines(c(1/n, 1- 1/n), c(1 - alpha, 1- alpha), col = "red", lty = "dashed")
      text(0.5, CL + 0.05, paste("Targeted Confidence Level =", CL))
}


## ---- echo = FALSE, out.height = '80%',out.width = '90%'--------------------------------
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
plot(pp, p.cov, type = "l", ylim =c(0.60, 1.1), main = paste("n = ", n), 
     xlab = "p", ylab = "Coverage Probability")
lines(c(1/n, 1- 1/n), c(1 - alpha, 1- alpha), col = "red", lty = "dashed")
      text(0.5, CL + 0.05, paste("Targeted Confidence Level =", CL))
}


## ---- eval = FALSE----------------------------------------------------------------------
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
plot(pp, p.cov, type = "l", ylim =c(0.60, 1.1), main = paste("n = ", n), 
     xlab = "p", ylab = "Coverage Probability")
lines(c(1/n, 1- 1/n), c(1 - alpha, 1- alpha), col = "red", lty = "dashed")
      text(0.5, CL + 0.05, paste("Targeted Confidence Level =", CL))
}


## ---- echo = FALSE, out.height = '80%', out.width = '90%'-------------------------------
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
plot(pp, p.cov, type = "l", ylim =c(0.60, 1.1), main = paste("n = ", n), 
     xlab = "p", ylab = "Coverage Probability")
lines(c(1/n, 1- 1/n), c(1 - alpha, 1- alpha), col = "red", lty = "dashed")
      text(0.5, CL + 0.05, paste("Targeted Confidence Level =", CL))
}


## ----eval = FALSE-----------------------------------------------------------------------
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
plot(pp, p.cov, type = "l", ylim =c(0.60, 1.1), main = paste("n = ", n), 
     xlab = "p", ylab = "Coverage Probability")
lines(c(1/n, 1- 1/n), c(1 - alpha, 1- alpha), col = "red", lty = "dashed")
      text(0.5, CL + 0.05, paste("Targeted Confidence Level =", CL))
}


## ----echo = FALSE, out.height = '80%',out.width = '90%'---------------------------------
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
plot(pp, p.cov, type = "l", ylim =c(0.60, 1.1), main = paste("n = ", n), 
     xlab = "p", ylab = "Coverage Probability")
lines(c(1/n, 1- 1/n), c(1 - alpha, 1- alpha), col = "red", lty = "dashed")
      text(0.5, CL + 0.05, paste("Targeted Confidence Level =", CL))
}


## ----echo=TRUE,warning=FALSE, message=FALSE,out.height = '40%',out.width = '70%', fig.align='center'----
library(resampledata)
head(Bangladesh)


## ----echo=TRUE,warning=FALSE, message=FALSE,out.height = '40%',out.width = '60%', fig.align='center'----
ggplot(data = Bangladesh, aes(x = Arsenic)) + 
  geom_histogram(fill = "blue", color = "black",
                 binwidth = 100) + 
  theme_bw()


## ----echo=TRUE,warning=FALSE, message=FALSE,out.height = '40%',out.width = '60%', fig.align='center'----
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
(BPCI <- quantile(Xbarstar, probs = c(0.025, 0.975)))


## ----echo=TRUE,eval=FALSE,warning=FALSE, message=FALSE,out.height = '90%',out.width = '90%', fig.align='center'----
par(mfrow= c(2, 2))
plot(Xbarstar, Sstar, ylab = "S*", xlab = substitute(paste(bar(X),"*")), col = rgb(1,0,0,0.01))
qqnorm(Tstar, col = rgb(1,0,0,0.01))
qqline(Tstar)
hist(Tstar, xlab = "T*", main = "Bootstrap distribution of T*", col = "red", breaks = "Scott")
hist(Xbarstar, xlab = substitute(paste(bar(X),"*")), 
     main = substitute(paste("Bootstrap Distribution of ", bar(X),"*")), col = "red", breaks = "Scott")


## ----echo=FALSE,warning=FALSE, message=FALSE,out.height = '80%',out.width = '90%', fig.align='center'----
par(mfrow= c(2, 2))
plot(Xbarstar, Sstar, ylab = "S*", xlab = substitute(paste(bar(X),"*")), col = rgb(1,0,0,0.01))
qqnorm(Tstar, col = rgb(1,0,0,0.01))
qqline(Tstar)
hist(Tstar, xlab = "T*", main = "Bootstrap distribution of T*", col = "red", breaks = "Scott")
hist(Xbarstar, xlab = substitute(paste(bar(X),"*")), main = substitute(paste("Bootstrap Distribution of ", bar(X),"*")), col = "red", breaks = "Scott")


## ---------------------------------------------------------------------------------------
LL <- xbar - CIt[2]*S/sqrt(n)
UL <- xbar - CIt[1]*S/sqrt(n)
(c(LL, UL))


## ----echo=TRUE,warning=FALSE, message=FALSE,out.height = '80%',out.width = '90%', fig.align='center'----
require(boot)
mean.boot <- function(data, i){
  d <- data[i]
  M <- mean(d)
  V <- var(d)/length(i)
  return(c(M, V))
}
boot.out <- boot(Arsenic, mean.boot, R=10^5)
boot.ci(boot.out, conf = 0.95, type = c("perc", "stud"))


## ----echo=TRUE,warning=FALSE, message=FALSE,out.height = '45%',out.width = '80%', fig.align='center'----
plot(boot.out)


## ----echo=TRUE,warning=FALSE, message=FALSE,out.height = '45%',out.width = '80%', fig.align='center'----
par(mfrow = c(1, 2))
hist(boot.out$t[,1], col = "pink", breaks = "Scott", main = "", 
     xlab = substitute(paste(bar(X),"*")), freq= FALSE)
lines(density(boot.out$t[,1]), lwd = 2)
hist((boot.out$t[,1] - boot.out$t0[1])/(boot.out$t[,2])^.5, col = "pink", breaks = "Scott", main = "", 
     xlab ="T*", freq= FALSE)
lines(density((boot.out$t[,1] - boot.out$t0[1])/(boot.out$t[,2])^.5), lwd = 2)


## ----echo=TRUE,warning=FALSE, message=FALSE,out.height = '45%',out.width = '80%', fig.align='center'----
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


## ----echo=TRUE,warning=FALSE, message=FALSE,out.height = '45%',out.width = '80%', fig.align='center'----
CItboot <- thetahat - quantile(Tstar, c(.975, .025)) * SE
names(CItboot) <- NULL
CItboot
CIperct <- quantile(DM, c(0.025, 0.975))
CIperct
t.test(Time.ILEC, Time.CLEC)$conf


## ----echo=TRUE,warning=FALSE, message=FALSE,out.height = '45%',out.width = '80%', fig.align='center'----
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


## ----echo=TRUE,warning=FALSE, message=FALSE,out.height = '45%',out.width = '80%', fig.align='center'----
plot(boot.out)


## ----echo=TRUE,warning=FALSE, message=FALSE,out.height = '45%',out.width = '80%', fig.align='center'----
par(mfrow = c(1, 2))
hist(boot.out$t[,1], col = "pink", breaks = "Scott", 
     main = "", freq= FALSE, xlab = substitute(paste(bar(x)[1],"* - ", bar(x)[2],"*")))
lines(density(boot.out$t[,1]), lwd = 2)
hist((boot.out$t[,1] - boot.out$t0[1])/(boot.out$t[,2])^.5, col = "pink", breaks = "Scott", 
     main = "", xlab ="T*", freq= FALSE)
lines(density((boot.out$t[,1] - boot.out$t0[1])/(boot.out$t[,2])^.5), lwd = 2)


## ---------------------------------------------------------------------------------------
library(tidyverse)
library(infer)
library(moderndive)
library(nycflights13)
library(ggplot2movies)


## ---------------------------------------------------------------------------------------
promotions %>% 
  sample_n(size = 10) %>% 
  arrange(id)


## ---- out.height = '45%', out.width = '70%'---------------------------------------------
ggplot(promotions, aes(x = gender, fill = decision)) +
  geom_bar() +
  labs(x = "Gender of name on résumé")


## ---------------------------------------------------------------------------------------
promotions %>% 
  group_by(gender, decision) %>% 
  tally()


## ----echo = FALSE, out.height = '40%',out.width = '70%'---------------------------------
knitr::include_graphics("week12_1.png")


## ---- echo = FALSE, out.height = '40%',out.width = '70%'--------------------------------
knitr::include_graphics("week12_2.jpeg")


## ----echo = FALSE, out.height = '40%',out.width = '70%'---------------------------------
knitr::include_graphics("week12_3.jpeg")


## ----echo=TRUE, warning=FALSE, message=FALSE,out.height = '40%',out.width = '70%', fig.align='center'----
ggplot(promotions_shuffled, 
       aes(x = gender, fill = decision)) +
  geom_bar() + 
  labs(x = "Gender of résumé name")


## ---------------------------------------------------------------------------------------
promotions_shuffled %>% 
  group_by(gender, decision) %>% 
  tally() # Same as summarize(n = n())


## ----echo = FALSE, out.height = '40%',out.width = '70%'---------------------------------
# knitr::include_graphics("week12_4.png")
set.seed(21)
promotions %>% 
  specify(formula = decision ~ gender, success = "promoted") %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 16, type = "permute") %>% 
  calculate(stat = "diff in props", order = c("male", "female")) -> ju
  ggplot(data = ju, aes(x = stat)) +
  geom_histogram(binwidth = 0.05, color = "white") + 
  xlim(-.4 ,.4) + 
  labs(x = "Difference in promotion rates (male - female)") +
  theme_bw() + 
  geom_vline(xintercept = .292, color = "red")


## ---------------------------------------------------------------------------------------
set.seed(37)
promotions %>% 
  specify(formula = decision ~ gender, success = "promoted") %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in props", order = c("male", "female")) -> null_distribution
get_pvalue(null_distribution, obs_stat= .292, direction = "right") -> pv
pv

### Consider doing the same thing with a for loop

P <- 10^3
pd <- numeric(P)
for(i in 1:P){
  pd[i] <- -diff(prop.table(table(promotions$decision, sample(promotions$gender)), 2)[2, ])
}

pvalue2 <- mean(pd >= 0.292)
pvalue2

## ----echo = FALSE, out.height = '50%',out.width = '70%'---------------------------------
knitr::include_graphics("week12_5.png")


## ---- echo = FALSE, out.height = '40%',out.width = '60%', fig.align='center'------------
#knitr::include_graphics("week12_6.png")
ggplot(data = null_distribution, aes(x = stat)) +
  geom_histogram(binwidth = 0.1, color = "white") + 
  xlim(-.4 ,.4) + 
  labs(x = "Difference in promotion rates (male - female)") +
  theme_bw() + 
  geom_vline(xintercept = .292, color = "red", linetype = "dashed")


## ---- echo = FALSE, out.height = '50%',out.width = '70%', fig.align='center'------------
knitr::include_graphics("week12_7.png")


## ---------------------------------------------------------------------------------------
promotions %>% 
  specify(formula = decision ~ gender, success = "promoted")


## ---- eval = FALSE----------------------------------------------------------------------
promotions %>% 
  specify(formula = decision ~ gender, 
          success = "promoted") %>% 
  hypothesize(null = "independence")


## ---- eval = FALSE----------------------------------------------------------------------
promotions_generate <- promotions %>% 
  specify(formula = decision ~ gender, 
          success = "promoted") %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute")


## ---------------------------------------------------------------------------------------
set.seed(37)
promotions %>% 
  specify(formula = decision ~ gender, success = "promoted") %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in props", order = c("male", "female")) -> null_distribution
null_distribution


## ---------------------------------------------------------------------------------------
obs_diff_prop <- promotions %>% 
  specify(decision ~ gender, success = "promoted") %>% 
  calculate(stat = "diff in props", 
            order = c("male", "female"))
obs_diff_prop


## ----echo=TRUE, out.height = '40%',out.width = '70%', fig.align='center'----------------
visualize(null_distribution, bins = 10) + 
  shade_p_value(obs_stat = obs_diff_prop, direction = "right") + 
  theme_bw()


## ---------------------------------------------------------------------------------------
null_distribution %>% 
  get_p_value(obs_stat = 0.292, direction = "right") 
# Or
mean(null_distribution$stat >= 0.292)


## ---- eval = FALSE----------------------------------------------------------------------
set.seed(37)
null_distribution <- promotions %>% 
  specify(formula = decision ~ gender, 
          success = "promoted") %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in props", 
            order = c("male", "female"))


## ---------------------------------------------------------------------------------------
bootstrap_distribution <- promotions %>% 
  specify(formula = decision ~ gender, 
          success = "promoted") %>% 
  # Change 1 - Remove hypothesize():
  # hypothesize(null = "independence") %>% 
  # Change 2 - Switch type from "permute" to "bootstrap":
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "diff in props", 
            order = c("male", "female"))


## ----echo=TRUE, warning=FALSE, message=FALSE,out.height = '35%',out.width = '70%', fig.align='center'----
percentile_ci <- bootstrap_distribution %>% 
  get_confidence_interval(level = 0.95, type = "percentile")
percentile_ci
visualize(bootstrap_distribution) + 
  shade_confidence_interval(endpoints = percentile_ci) + 
  theme_bw()


## ---- echo = FALSE, out.height = '50%',out.width = '70%'--------------------------------
knitr::include_graphics("week12_8.png")


## ---- echo = FALSE, out.height = '30%',out.width = '60%'--------------------------------
knitr::include_graphics("week12_9a.png")


## ---- echo = FALSE, out.height = '30%',out.width = '60%'--------------------------------
knitr::include_graphics("week12_9b.png")


## ----echo=FALSE, out.width = '100%', fig.pos="h", fig.align='center'--------------------
knitr::include_graphics("week13_2.png")


## ----echo=FALSE, out.width = '100%', fig.pos="h", fig.align='center'--------------------
knitr::include_graphics("week13_2.png")


## ----echo=FALSE, out.width = '100%', fig.pos="h", fig.align='center'--------------------
knitr::include_graphics("week13_3.png")

