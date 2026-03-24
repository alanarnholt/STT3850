## ----MEDskip, echo = FALSE------------------------------------------
library(knitr)
knit_hooks$set(document = function(x){
gsub("\\begin{tabular}", "\\medskip{}\\begin{tabular}", x, fixed = TRUE)
})


## ----setup, include = FALSE-----------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = NA, warning = FALSE, message = FALSE, fig.align = 'center')


## -------------------------------------------------------------------
library(tidyverse)
library(moderndive)
library(infer)
library(resampledata)


## ----eval = FALSE---------------------------------------------------
# set.seed(11)
# par(mfrow = c(3, 2))
# curve(dnorm(x, 25, 7), from = 25 - 2.5*7, 25 + 2.5*7, col = "blue", main = "N(25, 7)", ylab = "", xlab = "")
# abline(v = 25, col = "red")
# curve(dnorm(x, 25, 1), from = 25 - 2.5*7, 25 + 2.5*7, col = "blue", main = "N(25, 1)", ylab = "", xlab = "")
# abline(v = 25, col = "red")
# rs1 <- rnorm(49, 25, 7)
# rs2 <- rnorm(49, 25, 7)
# hist(rs1, xlab = "", main = "n = 49")
# abline(v = mean(rs1), col = "red")
# B <- 10000
# my.boot.stat1 <- numeric(B)
# my.boot.stat2 <- numeric(B)
# for (i in 1:B){
#   x1 <- sample(rs1, size = 49, replace = TRUE)
#   x2 <- sample(rs2, size = 49, replace = TRUE)
#   my.boot.stat1[i] <- mean(x1)
#   my.boot.stat2[i] <- mean(x2)
# }
# hist(my.boot.stat1, breaks = "Scott",  main ="Bootstrap Distribution", freq= FALSE, xlab = "",
# xlim = c(25 - 2.5*7, 25 + 2.5*7))
# abline(v = mean(rs1), col = "red")
# hist(rs2, xlab = "", main = "n = 49")
# abline(v = mean(rs2), col = "red")
# hist(my.boot.stat2, breaks = "Scott",  main ="Bootstrap Distribution", freq= FALSE, xlab = "",
# xlim = c(25 - 2.5*7, 25 + 2.5*7))
# abline(v = mean(rs2), col = "red")
# c(mean(rs1), sd(rs1), mean(rs2), sd(rs2),
#   mean(my.boot.stat1), sd(my.boot.stat1), mean(my.boot.stat2), sd(my.boot.stat2))


## ----echo = FALSE, out.height = '75%', out.width = '90%'------------
set.seed(11)
par(mfrow = c(3, 2))
curve(dnorm(x, 25, 7), from = 25 - 2.5*7, 25 + 2.5*7, col = "blue", main = "N(25, 7)", ylab = "", xlab = "")
abline(v = 25, col = "red")
curve(dnorm(x, 25, 1), from = 25 - 2.5*7, 25 + 2.5*7, col = "blue", main = "N(25, 1)", ylab = "", xlab = "")
abline(v = 25, col = "red")
rs1 <- rnorm(49, 25, 7); rs2 <- rnorm(49, 25, 7)
hist(rs1, xlab = "", main = "n = 49")
abline(v = mean(rs1), col = "red")
B <- 10000
my.boot.stat1 <- numeric(B); my.boot.stat2 <- numeric(B)
for (i in 1:B){
  x1 <- sample(rs1, size = 49, replace = TRUE) 
  x2 <- sample(rs2, size = 49, replace = TRUE) 
  my.boot.stat1[i] <- mean(x1)
  my.boot.stat2[i] <- mean(x2)
}
hist(my.boot.stat1, breaks = "Scott",  main ="Bootstrap Distribution", freq= FALSE, xlab = "", 
xlim = c(25 - 2.5*7, 25 + 2.5*7))
abline(v = mean(rs1), col = "red")
hist(rs2, xlab = "", main = "n = 49")
abline(v = mean(rs2), col = "red")
hist(my.boot.stat2, breaks = "Scott",  main ="Bootstrap Distribution", freq= FALSE, xlab = "", 
xlim = c(25 - 2.5*7, 25 + 2.5*7))
abline(v = mean(rs2), col = "red")
c(mean(rs1), sd(rs1), mean(rs2), sd(rs2), 
  mean(my.boot.stat1), sd(my.boot.stat1), mean(my.boot.stat2), sd(my.boot.stat2))


## ----echo = TRUE, eval = FALSE--------------------------------------
# set.seed(281)
# par(mfrow = c(3, 2))
# curve(dgamma(x, 1, 1/2), from = 0, to = 8, col = "blue", main = "Gamma(1, 1/2)", ylab = "", xlab = "")
# abline(v = 2, col = "red")
# curve(dgamma(x, 16, 8), from = 0, 8, col = "blue", main = "Gamma(16, 8)", ylab = "", xlab = "")
# abline(v = 2, col = "red")
# rsg1 <- rgamma(16, 1, 1/2)
# rsg2 <- rgamma(16, 1, 1/2)
# hist(rsg1, xlab = "", main = "n = 16", xlim = c(0, 8))
# abline(v = mean(rsg1), col = "red")
# B <- 10000
# my.boot.statg1 <- numeric(B)
# my.boot.statg2 <- numeric(B)
# for (i in 1:B){
#   xg1 <- sample(rsg1, size = 16, replace = TRUE)
#   xg2 <- sample(rsg2, size = 16, replace = TRUE)
#   my.boot.statg1[i] <- mean(xg1)
#   my.boot.statg2[i] <- mean(xg2)
# }
# hist(my.boot.statg1, breaks = "Scott",  main ="Bootstrap Distribution", freq= FALSE, xlab = "",
# xlim = c(0, 8))
# abline(v = mean(rsg1), col = "red")
# hist(rsg2, xlab = "", main = "n = 16", xlim = c(0, 8))
# abline(v = mean(rsg2), col = "red")
# hist(my.boot.statg2, breaks = "Scott",  main ="Bootstrap Distribution", freq= FALSE, xlab = "",
# xlim = c(0, 8))
# abline(v = mean(rsg2), col = "red")


## ----echo = FALSE, out.height = '75%', out.width = '90%'------------
set.seed(281)
par(mfrow = c(3, 2))
curve(dgamma(x, 1, 1/2), from = 0, to = 8, col = "blue", main = "Gamma(1, 1/2)", ylab = "", xlab = "")
abline(v = 2, col = "red")
curve(dgamma(x, 16, 8), from = 0, 8, col = "blue", main = "Gamma(16, 8)", ylab = "", xlab = "")
abline(v = 2, col = "red")
rsg1 <- rgamma(16, 1, 1/2)
rsg2 <- rgamma(16, 1, 1/2)
hist(rsg1, xlab = "", main = "n = 16", xlim = c(0, 8))
abline(v = mean(rsg1), col = "red")
B <- 10000
my.boot.statg1 <- numeric(B)
my.boot.statg2 <- numeric(B)
for (i in 1:B){
  xg1 <- sample(rsg1, size = 16, replace = TRUE)
  xg2 <- sample(rsg2, size = 16, replace = TRUE)
  my.boot.statg1[i] <- mean(xg1)
  my.boot.statg2[i] <- mean(xg2)
}
hist(my.boot.statg1, breaks = "Scott",  main ="Bootstrap Distribution", freq= FALSE, xlab = "", 
xlim = c(0, 8))
abline(v = mean(rsg1), col = "red")
hist(rsg2, xlab = "", main = "n = 16", xlim = c(0, 8))
abline(v = mean(rsg2), col = "red")
hist(my.boot.statg2, breaks = "Scott",  main ="Bootstrap Distribution", freq= FALSE, xlab = "", 
xlim = c(0, 8))
abline(v = mean(rsg2), col = "red")


## ----eval=FALSE-----------------------------------------------------
# par(mfrow = c(2, 2))
# Bang <- Bangladesh
# Arsenic <- Bang$Arsenic
# hist(Arsenic, breaks = "Scott", main = "Figure 1a", col = "lightblue")
# qqnorm(Arsenic, main = "Figure 1b")
# qqline(Arsenic, col = "red")
# B <- 10000
# n <- sum(!is.na(Arsenic))
# arsenic.mean <- numeric(B)
# set.seed(7)
# for (i in 1:B){
#   bss <- sample(Arsenic, size = n, replace = TRUE)
#   arsenic.mean[i] <- mean(bss)
# }
# hist(arsenic.mean, main = "Figure 2a", col = "lightblue", breaks = "Scott",
#      xlab = substitute(paste(bar(X),"*")))
# qqnorm(arsenic.mean, main = "Figure 2b")
# qqline(arsenic.mean, col = "red")


## ----echo = FALSE, out.height = '85%',out.width = '90%'-------------
par(mfrow = c(2, 2))
Bang <- Bangladesh
Arsenic <- Bang$Arsenic
hist(Arsenic, breaks = "Scott", main = "Figure 1a", col = "lightblue")
qqnorm(Arsenic, main = "Figure 1b")
qqline(Arsenic, col = "red")
B <- 10000
n <- sum(!is.na(Arsenic))
arsenic.mean <- numeric(B)
set.seed(7)
for (i in 1:B){
  bss <- sample(Arsenic, size = n, replace = TRUE)
  arsenic.mean[i] <- mean(bss)
}
hist(arsenic.mean, main = "Figure 2a", col = "lightblue", breaks = "Scott", xlab = substitute(paste(bar(X),"*")))
qqnorm(arsenic.mean, main = "Figure 2b")
qqline(arsenic.mean, col = "red")


## -------------------------------------------------------------------
pennies_sample |> 
  summarize(xbar_year = mean(year))


## ----echo = FALSE, out.height = '40%',out.width = '70%'-------------
knitr::include_graphics("week10_7.png")


## -------------------------------------------------------------------
pennies_sample |> 
  specify(response = year) |> 
  generate(reps = 1000, type = "bootstrap") |> 
  calculate(stat = "mean") -> bs_dist
bs_dist |> 
  summarize(lci = quantile(stat, probs = 0.025), 
            uci = quantile(stat, probs = 0.975)) -> CI
CI


## ----label = "bsci", fig.cap = "Bootstrap Distribution with percentile CI limits", out.height = '40%', out.width = '50%'----
get_confidence_interval(bs_dist, level = 0.95)
visualize(bs_dist) + 
  shade_confidence_interval(endpoints = CI)


## -------------------------------------------------------------------
# Using a for loop to do the same thing

set.seed(32)
B <- 1000
bm <- numeric(B)
for(i in 1:B){
  bss <- sample(pennies_sample$year, size = 50, replace = TRUE)
  bm[i] <- mean(bss)
}
quantile(bm, probs = c(0.025, 0.975))


## ----echo = TRUE----------------------------------------------------
set.seed(10)
virtual_resampled_means <- pennies_sample |> 
  rep_sample_n(size = 50, replace = TRUE, reps = 1000) |> 
  group_by(replicate) |> 
  summarize(mean_year = mean(year))
virtual_resampled_means |> 
  summarize(SE = sd(mean_year)) -> ans
ans
# Or
sd(bm)


## -------------------------------------------------------------------
mean(pennies_sample$year) +c(-1, 1)*qnorm(.975)*ans$SE


## ----out.height = '40%', out.width = '60%'--------------------------
set.seed(10)
virtual_resampled_means <- pennies_sample |> 
  rep_sample_n(size = 50, replace = TRUE, reps = 1000) |> 
  group_by(replicate) |> 
  summarize(mean_year = mean(year))
ggplot(virtual_resampled_means, aes(x = mean_year)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 1990) +
  labs(x = "sample mean") +
  theme_bw()


## -------------------------------------------------------------------
quantile(virtual_resampled_means$mean_year, 
         prob = c(0.025, 0.975))


## ----out.height = '50%', out.width = '70%'--------------------------
set.seed(10)
bootstrap_distribution <- pennies_sample |> 
  specify(response = year) |> 
  generate(reps = 1000, type = "bootstrap") |> 
  calculate(stat = "mean")
visualize(bootstrap_distribution)


## ----out.height = '40%',out.width = '70%'---------------------------
percentile_ci <- bootstrap_distribution |> 
  get_confidence_interval(level = 0.95, type = "percentile")
percentile_ci
visualize(bootstrap_distribution) + 
  shade_confidence_interval(endpoints = percentile_ci)


## ----out.height = '40%', out.width = '70%'--------------------------
x_bar <- pennies_sample |> summarize(mean_year = mean(year))
standard_error_ci <- bootstrap_distribution |> 
  get_confidence_interval(type = "se", point_estimate = x_bar, level = 0.95)
standard_error_ci
visualize(bootstrap_distribution) + 
  shade_confidence_interval(endpoints = standard_error_ci)


## ----out.height = '45%', out.width = '70%'--------------------------
library(resampledata)
Babies <- NCBirths2004
set.seed(13)
bsd <- Babies |> 
  specify(response = Weight) |> 
  generate(reps = 10^4, type = "bootstrap") |> 
  calculate(stat = "mean")
visualize(bsd)


## ----out.height = '45%', out.width = '70%'--------------------------
percentile_ci <- bsd |> 
  get_confidence_interval(level = 0.95, type = "percentile")
percentile_ci
visualize(bsd) + 
  shade_confidence_interval(endpoints = percentile_ci)


## ----out.height = '40%', out.width = '70%'--------------------------
x_bar_babies <- Babies |> summarize(Mean = mean(Weight))
standard_error_ci <- bsd |> 
  get_confidence_interval(type = "se", point_estimate = x_bar_babies, level = 0.95)
standard_error_ci
visualize(bsd) + 
  shade_confidence_interval(endpoints = standard_error_ci)


## -------------------------------------------------------------------
bowl |> summarize(p_red = mean(color == "red"))


## -------------------------------------------------------------------
head(bowl_sample_1, n = 3)
bowl_sample_1 |> 
  summarize(p_hat = mean(color == "red"))


## -------------------------------------------------------------------
set.seed(10)
sample_1_bootstrap <- bowl_sample_1 |> 
  specify(response = color, success = "red") |> 
  generate(reps = 1000, type = "bootstrap") |> 
  calculate(stat = "prop")
percentile_ci_1 <- sample_1_bootstrap |> 
  get_confidence_interval(level = 0.95, type = "percentile")
percentile_ci_1


## ----echo=TRUE, warning=FALSE, message=FALSE,out.height = '45%',out.width = '70%', fig.align='center'----
sample_1_bootstrap |> 
  visualize(bins = 15) + 
  shade_confidence_interval(endpoints = percentile_ci_1) +
  geom_vline(xintercept = 0.42, linetype = "dashed")


## ----echo = FALSE, out.height = '40%',out.width = '60%'-------------
knitr::include_graphics("week10_11.png")


## ----echo = FALSE, out.height = '50%', out.width = '60%'------------
knitr::include_graphics("week10_12.png")


## -------------------------------------------------------------------
library(resampledata)
library(tidyverse)
library(moderndive)
library(infer)
head(TV)


## -------------------------------------------------------------------
ct <- tapply(TV$Times, TV$Cable, mean)
ct
# Tidy approach
TV |>
  group_by(Cable) |>
  summarize(Means = mean(Times), n = n())


## -------------------------------------------------------------------
times.Basic <- subset(TV, select = Times, 
                      subset = Cable == "Basic", drop = TRUE)
times.Ext <- subset(TV, select = Times, 
                    subset = Cable == "Extended", drop = TRUE)
B <- 10^4
times.diff.mean <- numeric(B)
set.seed(5)
for (i in 1:B){
  Basic.sample <- sample(times.Basic, 
                  size = sum(!is.na(times.Basic)), replace = TRUE)
  Ext.sample <- sample(times.Ext,
                  size = sum(!is.na(times.Ext)), replace = TRUE)
  times.diff.mean[i] <- mean(Basic.sample) - mean(Ext.sample)
}
opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2))
CI <- quantile(times.diff.mean, prob = c(0.025, 0.975))
CI


## ----out.height = '43%', out.width = '70%'--------------------------
par(mfrow = c(1, 2))
hist(times.diff.mean, breaks = "Scott", freq=FALSE, 
     main = "Bootstrap Distribution \n (Figure a)", 
     xlab = substitute(paste(bar(x)[1],"*", - bar(x)[2],"*")), 
     col = "lightblue")
abline(v = c(0, CI), col = c("blue", "red", "red"), lwd = 2, 
       lty = c("solid", "dashed", "dashed"))
qqnorm(times.diff.mean, main = "Normal Q-Q Plot \n (Figure b)")
qqline(times.diff.mean, col = "red")
sd(times.diff.mean) -> SEbdm
SEbdm


## ----echo=TRUE, warning=FALSE, message=FALSE,out.height = '45%',out.width = '70%', fig.align='center'----
set.seed(5)
TV |> 
  specify(Times ~ Cable) |> 
  generate(reps = 10^4 - 1, type = "bootstrap") |> 
  calculate(stat = "diff in means", order = c("Basic", "Extended")) -> bootdist
visualize(bootdist) + theme_bw() +
  labs(x = substitute(paste(bar(x)[basic],"*", - bar(x)[extended],"*")))


## ----echo=TRUE, warning=FALSE, message=FALSE,out.height = '38%',out.width = '60%', fig.align='center'----
get_confidence_interval(bootdist, level = 0.95) -> CI2
CI2
###
visualize(bootdist) + theme_bw() +
  labs(x = substitute(paste(bar(x)[basic],"*", - bar(x)[extended],"*"))) + 
  shade_confidence_interval(endpoints = CI2) + 
  geom_vline(xintercept = 0, color = "purple", size = 2)


## -------------------------------------------------------------------
Phone <- Verizon
rt <- tapply(Phone$Time, Phone$Group, mean)
rt
# Tidy approach
Phone |> 
  group_by(Group) |> 
  summarize(Mean = mean(Time), n = n(), SD = sd(Time))


## ----out.height = '40%', out.width = '60%'--------------------------
par(mfrow = c(1, 2))
times.ILEC <- subset(Phone, select = Time, subset = Group == "ILEC", drop = TRUE)
B <- 10^4
ILECmean <- numeric(B)
set.seed(3)
for (i in 1:B){
 ILECmean[i] <- mean(sample(times.ILEC, size = length(times.ILEC), replace = TRUE)) 
}
opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2))
hist(ILECmean, breaks = "Scott", col = "lightblue", 
     main = "Bootstrap Distribution \n Figure a", 
     freq= FALSE, xlab = substitute(paste(bar(x),"*")))
qqnorm(ILECmean, main = "Normal Q-Q Plot \n Figure b")
qqline(ILECmean, col = "red")


## -------------------------------------------------------------------
sd(ILECmean)
CI <- quantile(ILECmean, prob = c(0.025, 0.975))
CI


## ----out.height = '35%', out.width = '60%'--------------------------
par(mfrow = c(1, 2))
times.CLEC <- subset(Phone, select = Time, subset = Group == "CLEC", drop = TRUE)
B <- 10^4
CLECmean <- numeric(B)
set.seed(2)
for (i in 1:B){
 CLECmean[i] <- mean(sample(times.CLEC, size = length(times.CLEC), replace = TRUE)) 
}
opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2))
hist(CLECmean, breaks = "Scott", col = "lightblue", 
     main = "Bootstrap Distribution \n Figure a", 
     freq= FALSE, xlab = substitute(paste(bar(x),"*")))
qqnorm(CLECmean, main = "Normal Q-Q Plot \n Figure b")
qqline(CLECmean, col = "red")


## -------------------------------------------------------------------
c(sd(CLECmean), mean(CLECmean))
CIC <- quantile(CLECmean, prob = c(0.025, 0.975))
CIC


## -------------------------------------------------------------------
B <- 10^4
diffmeans <- numeric(B)
set.seed(1)
for (i in 1:B){
  ILEC.sample <- sample(times.ILEC, size = length(times.ILEC), 
                        replace = TRUE)
  CLEC.sample <- sample(times.CLEC, size = length(times.CLEC), 
                        replace = TRUE)
  diffmeans[i] <- mean(ILEC.sample) - mean(CLEC.sample)
}
CIdiff <- quantile(diffmeans, prob = c(0.025, 0.975))
CIdiff


## ----out.height = '40%', out.width = '60%'--------------------------
par(mfrow=c(1, 2))
hist(diffmeans, breaks = "Scott", col = "lightblue", 
     main = "Bootstrap Distribution \n Figure a", 
     freq= FALSE, xlab = substitute(paste(bar(x)[ILEC],"*", - bar(x)[CLEC],"*")))
abline(v = c(CIdiff, 0), col = c("blue", "blue", "red"), lwd = 2, 
       lty = c("dashed", "dashed", "solid"))
qqnorm(diffmeans, main = "Normal Q-Q Plot \n Figure b")
qqline(diffmeans, col = "red")
c(mean(diffmeans), sd(diffmeans))


## -------------------------------------------------------------------
B <- 10^4
diffmeans.25 <- numeric(B)
set.seed(3)
for (i in 1:B){
  ILEC.sample <- sample(times.ILEC, size = length(times.ILEC), replace = TRUE)
  CLEC.sample <- sample(times.CLEC, size = length(times.CLEC), replace = TRUE)
  diffmeans.25[i] <- mean(ILEC.sample, trim = .25) - mean(CLEC.sample, trim = .25)
}
CIdiff.25 <- quantile(diffmeans.25, prob = c(0.025, 0.975))
CIdiff.25


## ----out.height = '40%', out.width = '60%'--------------------------
par(mfrow=c(1, 2))
hist(diffmeans.25, breaks = "Scott", col = "lightblue", 
     main = "Bootstrap Distribution \n Figure 14a \n 0.25 Trimmed Means", 
     freq= FALSE, xlab = substitute(paste(bar(x)[1],"*", - bar(x)[2],"*")))
abline(v = c(CIdiff.25, 0), col = c("blue", "blue", "red"), 
       lty = c("dashed", "dashed", "solid"))
qqnorm(diffmeans.25, main = "Normal Q-Q Plot \n Figure 14b")
qqline(diffmeans.25, col = "red")
sd(diffmeans.25)


## -------------------------------------------------------------------
library(moderndive)
library(tidyverse)
library(infer)
mythbusters_yawn |> 
  group_by(group, yawn) |> 
  summarize(count = n())


## ----out.height = '40%', out.width = '60%'--------------------------
set.seed(10)
bootstrap_distribution_yawning <- mythbusters_yawn |> 
  specify(formula = yawn ~ group, success = "yes") |> 
  generate(reps = 1000, type = "bootstrap") |> 
  calculate(stat = "diff in props", order = c("seed", "control"))
visualize(bootstrap_distribution_yawning) +
  geom_vline(xintercept = 0)


## -------------------------------------------------------------------
percentile_ci<-bootstrap_distribution_yawning |> 
  get_confidence_interval(type = "percentile", level = 0.95)
percentile_ci
obs_diff_in_props <- mythbusters_yawn |> 
  specify(formula = yawn ~ group, success = "yes") |> 
  calculate(stat = "diff in props", order = c("seed", "control"))
obs_diff_in_props
myth_ci_se <- bootstrap_distribution_yawning |> 
  get_confidence_interval(type = "se", point_estimate = obs_diff_in_props,level = 0.95)
myth_ci_se


## -------------------------------------------------------------------
myth_ci_se <- bootstrap_distribution_yawning |> 
  get_confidence_interval(type = "se", 
                          point_estimate = obs_diff_in_props,
                          level = 0.95)
myth_ci_se


## ----out.height = '40%', out.width = '60%'--------------------------
visualize(bootstrap_distribution_yawning) +
shade_confidence_interval(endpoints = percentile_ci)


## ----echo=TRUE, eval=FALSE------------------------------------------
# set.seed(13)
# counter <- 0 # set counter to 0
# mu <- 25
# sigma <- 4
# n <- 30
# sims <- 10^4
# plot(x = c(mu - 4*sigma/sqrt(n), mu + 4*sigma/sqrt(n)),
#      y = c(1, 100), type = "n", xlab = "", ylab = "")
# for (i in 1:sims){
#  x <- rnorm(n, mu, sigma)
#  L <- mean(x) - qnorm(0.975)*sigma/sqrt(n)
#  U <- mean(x) - qnorm(0.025)*sigma/sqrt(n)
#  if(L < mu && mu < U){counter <- counter + 1}
#  if(i <= 100){
#  segments(L, i, U, i, col = "blue")
#  }
# }
# abline(v = mu, col = "red")


## ----echo=FALSE, out.height = '70%',out.width = '60%'---------------
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


## ----echo=TRUE, warning=FALSE, message=FALSE,out.height = '30%',out.width = '40%', fig.align='center'----
ACL <- counter/sims*100
ACL


## ----out.height = '40%',out.width = '60%'---------------------------
library(PASWR2)
set.seed(11)
cisim(samples = 100, n = 30, parameter = 25, sigma = 4, 
      type = "Mean")


## -------------------------------------------------------------------
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


## -------------------------------------------------------------------
n <- ceiling((qnorm(.975)*24.6/5)^2)
n
# Using nsize from PASWR2
nsize(b = 5, sigma = 24.6, conf.level = 0.95, type = "mu")


## ----out.height = '40%',out.width = '60%'---------------------------
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


## ----out.height = '40%', out.width = '60%'--------------------------
# Consider qq plot for t_15
ggplot(data = data.frame(x = TS), aes(sample = x)) + 
  geom_qq(distribution = stats::qt, dparams = list(df = 15), size = 0.1, color = "blue") + 
  geom_abline(intercept = 0, slope = 1, color = "pink") + 
  theme_bw()


## ----out.height = '40%', out.width = '60%'--------------------------
curve(dnorm(x, 0, 1), -4, 4, col = "black", ylab = "", xlab = "")
curve(dt(x, 1), add = TRUE, lty = 2, col = "green")
curve(dt(x, 4), add = TRUE, lty = 3, col = "pink")
curve(dt(x, 9), add = TRUE, lty = 4, col = "red")
curve(dt(x, 36), add = TRUE, lty = 5, col = "blue")
abline(h = 0, lwd=2)
legend("topright", legend = c("N(0, 1)", "t_1", "t_4", "t_9", "t_36"), 
       lty = c(1, 2, 3, 4, 5), col =c("black", "green", "pink", "red", "blue"), 
       lwd = 1.5)


## ----out.height = '40%', out.width = '60%'--------------------------
ggplot(data = data.frame(x = c(-5, 5)), aes(x = x)) + 
  theme_bw() +
  labs(x = "", y = "") +
  stat_function(fun = dt, args = list(df = 1), n = 200, color = "green", linetype = "dashed") + 
  stat_function(fun = dt, args = list(df = 4), n = 200, color = "pink", linetype = "dashed") + 
  stat_function(fun = dt, args = list(df = 9), n = 200, color = "red", linetype = "dashed") + 
  stat_function(fun = dt, args = list(df = 36), n = 200, color = "blue", linetype = "dashed") + 
  stat_function(fun = dnorm, n = 200, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 0)


## -------------------------------------------------------------------
qt(.95, 27)


## -------------------------------------------------------------------
# Using function from PASWR2
tsum.test(mean.x = 110, s.x = 7.5, n.x = 28, 
          conf.level = 0.90)$conf


## -------------------------------------------------------------------
library(resampledata)
head(NCBirths2004, n = 2)
NCBirths2004 |>group_by(Gender)|> 
  summarize(Mean = mean(Weight),SD=sd(Weight), n = n()) -> BW
BW


## ----out.height = '50%', out.width = '70%'--------------------------
# Using lattice
qqmath(~Weight|Gender, data = NCBirths2004, col = rgb(1, 0, 0, 0.1))


## ----out.height = '50%', out.width = '70%'--------------------------
ggplot(data = NCBirths2004, aes(sample = Weight)) + 
  stat_qq(color = rgb(1, 0, 0, 0.1)) + 
  stat_qq_line() +
  facet_grid(cols = vars(Gender)) + 
  theme_bw()


## -------------------------------------------------------------------
qt(0.995, 520)


## -------------------------------------------------------------------
# t.test() to find confidence intervals.
t.test(NCBirths2004$Weight[NCBirths2004$Gender=="Female"], 
       conf = 0.99)$conf
# Or
JG <- NCBirths2004 |> 
  filter(Gender == "Female") 
t.test(JG$Weight, conf = 0.99)$conf


## -------------------------------------------------------------------
girls <- subset(NCBirths2004, select = Weight, 
                subset = Gender =="Female", drop = TRUE)
B <- 10^4
bsmean <- numeric(B)
for(i in 1:B){
  bss <- sample(girls, size = length(girls), replace = TRUE)
  bsmean[i] <- mean(bss)}
(CIperc <- quantile(bsmean, probs = c(0.005, 0.995)))
(CIse <- c(mean(girls) + 
    c(-1, 1)*qt(.995, length(girls) - 1)*sd(bsmean)))


## ----out.height = '40%', out.width = '60%'--------------------------
set.seed(13)
x <- rgamma(n=1000, shape=5, rate=2)
#create histogram to view distribution of values
hist(x, main="")


## -------------------------------------------------------------------
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


## ----echo = TRUE, eval = FALSE--------------------------------------
# set.seed(13); library(gridExtra)
# n <- 10           # sample size
# q <- qt(0.975, n - 1)
# N <- 10^5
# TSU <- numeric(N)
# for(i in 1:N){
#   x <- runif(n, 0, 1)
#   xbar <- mean(x)
#   s <- sd(x)
#   TSU[i] <- (xbar - 0.5)/(s/sqrt(n))
# }
# TSE10 <- numeric(N)
# for(i in 1:N){
#   x <- rexp(n, 1)
#   xbar <- mean(x)
#   s <- sd(x)
#   TSE10[i] <- (xbar - 1)/(s/sqrt(n))
# }
# n <- 10
# p1 <- qqmath(~TSU, col = "red", xlim = c(-3,3), ylim = c(-3,3), distribution = function(p){qt(p, df = n - 1)},
#        xlab = "Theoretical t quantiles", ylab = "Sample quantiles", main = "Uniform, n = 10",
#        panel = function(x,...){
#   panel.qqmath(x, pch = ".", ...)
#   panel.abline(a = 0, b =1, ...)})
# p2 <- qqmath(~TSE10, col = "red", xlim = c(-3,3), ylim = c(-3,3), distribution = function(p){qt(p, df = n - 1)},
#        xlab = "Theoretical t quantiles", ylab = "Sample quantiles", main = "Exponential, n = 10",
#        panel = function(x,...){
#   panel.qqmath(x, pch = ".", ...)
#   panel.abline(a = 0, b = 1, ...)})
# gridExtra::grid.arrange(p1, p2, ncol = 2)


## ----echo = FALSE, out.height = '50%',out.width = '80%'-------------
set.seed(13); library(gridExtra)
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
n <- 10
p1<-qqmath(~TSU, col = "red", xlim = c(-3,3), ylim = c(-3,3), distribution = function(p){qt(p, df = n - 1)}, 
       xlab = "Theoretical t quantiles", ylab = "Sample quantiles", main = "Uniform, n = 10", 
       panel = function(x,...){
  panel.qqmath(x, pch = ".", ...)
  panel.abline(a = 0, b =1, ...)})
p2<-qqmath(~TSE10, col = "red", xlim = c(-3,3), ylim = c(-3,3), distribution = function(p){qt(p, df = n - 1)}, 
       xlab = "Theoretical t quantiles", ylab = "Sample quantiles", main = "Exponential, n = 10", 
       panel = function(x,...){
  panel.qqmath(x, pch = ".", ...)
  panel.abline(a = 0, b = 1, ...)})
gridExtra::grid.arrange(p1, p2, ncol = 2)


## ----echo = TRUE, eval = FALSE--------------------------------------
# set.seed(13); library(gridExtra)
# n <- 10           # sample size
# q <- qt(0.975, n - 1)
# N <- 10^5
# n <- 100
# TSE100 <- numeric(N)
# for(i in 1:N){
#   x <- rexp(n, 1)
#   xbar <- mean(x)
#   s <- sd(x)
#   TSE100[i] <- (xbar - 1)/(s/sqrt(n))}
# n <- 5000
# TSE5000 <- numeric(N)
# for(i in 1:N){
#   x <- rexp(n, 1)
#   xbar <- mean(x)
#   s <- sd(x)
#   TSE5000[i] <- (xbar - 1)/(s/sqrt(n))}
# n <- 100
# p1<-qqmath(~TSE100,col = "red", xlim = c(-3,3), ylim = c(-3,3), distribution = function(p){qt(p, df = n - 1)},
#        xlab = "Theoretical t quantiles", ylab = "Sample quantiles", main = "Exponential, n = 100",
#        panel = function(x,...){
#   panel.qqmath(x, pch = ".", ...)
#   panel.abline(a = 0, b = 1, ...)})
# n <- 100
# p2<-qqmath(~TSE5000,col = "red", xlim = c(-3,3), ylim = c(-3,3), distribution = function(p){qt(p, df = n - 1)},
#        xlab = "Theoretical t quantiles", ylab = "Sample quantiles", main = "Exponential, n = 5000",
#        panel = function(x,...){
#   panel.qqmath(x, pch = ".", ...)
#   panel.abline(a = 0, b = 1, ...)})
# gridExtra::grid.arrange(p1, p2, ncol = 2)


## ----echo = FALSE, out.height = '50%', out.width = '80%'------------
set.seed(13); library(gridExtra)
n <- 10           # sample size
q <- qt(0.975, n - 1)
N <- 10^5
n <- 100
TSE100 <- numeric(N)
for(i in 1:N){
  x <- rexp(n, 1)
  xbar <- mean(x)
  s <- sd(x)
  TSE100[i] <- (xbar - 1)/(s/sqrt(n))}
n <- 5000
TSE5000 <- numeric(N)
for(i in 1:N){
  x <- rexp(n, 1)
  xbar <- mean(x)
  s <- sd(x)
  TSE5000[i] <- (xbar - 1)/(s/sqrt(n))}
n <- 100
p1<-qqmath(~TSE100,col = "red", xlim = c(-3,3), ylim = c(-3,3), distribution = function(p){qt(p, df = n - 1)}, 
       xlab = "Theoretical t quantiles", ylab = "Sample quantiles", main = "Exponential, n = 100", 
       panel = function(x,...){
  panel.qqmath(x, pch = ".", ...)
  panel.abline(a = 0, b = 1, ...)})
n <- 100
p2<-qqmath(~TSE5000,col = "red", xlim = c(-3,3), ylim = c(-3,3), distribution = function(p){qt(p, df = n - 1)}, 
       xlab = "Theoretical t quantiles", ylab = "Sample quantiles", main = "Exponential, n = 5000", 
       panel = function(x,...){
  panel.qqmath(x, pch = ".", ...)
  panel.abline(a = 0, b = 1, ...)})
gridExtra::grid.arrange(p1, p2, ncol = 2)

