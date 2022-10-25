## ----sop, include=FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = NA, warning = FALSE, message = FALSE, fig.align = "center")
library(tidyverse)
library(infer)


## ----SETUP------------------------------------------------------------------------------------------
library(resampledata)
library(xtable)
Babies <- NCBirths2004
Babies %>% 
  summarize(Mean = mean(Weight), n = n())


## ----BS, comment = NA-------------------------------------------------------------------------------
rs <- c(3969, 3204, 2892)
xstar1 <- rep(rs, each = 9)
xstar2 <- rep(rep(rs, each = 3), 3)
xstar3 <- rep(rs, 9)
XD <- data.frame(xstar1, xstar2, xstar3)
xbarstar <- apply(XD, 1, mean)
BD <- cbind(XD, xbarstar)
WD <- BD


## ---- label = "BST1", echo = FALSE------------------------------------------------------------------
knitr::kable(WD, booktabs = TRUE, col.names = c("$x^*_1$", "$x^*_2$", "$x^*_3$", "$\\bar{x}^*$"), caption = "All possible Samples of Size 3 from 3969, 3204, and 2892")


## ----BT, results='asis', echo =FALSE, eval = FALSE--------------------------------------------------
## colnames(WD) <- c("$x^*_1$", "$x^*_2$", "$x^*_3$", "$\\bar{x}^*$")
## print(xtable(WD, caption = "Table 1 --- All possible Samples of Size 3 from 3969, 3204, and 2892"), type = "html", include.rownames=FALSE)


## ----BSTAB------------------------------------------------------------------------------------------
head(BD)
mean(rs)
mean(BD[,"xbarstar"])
# or
mean(BD$xbarstar)
# tidyverse
BD %>%
  summarize(BootMean = mean(xbarstar), BootSD = sd(xbarstar))


## ----boot1------------------------------------------------------------------------------------------
library(ggplot2)
set.seed(13)
head(Babies)
B <- 10000
bsMean <- numeric(B)
for (i in 1:B){
  bss <- sample(Babies$Weight, size = sum(!is.na(Babies$Weight)), replace = TRUE) 
  bsMean[i] <- mean(bss)
}
hist(bsMean, breaks = "Scott", col = 'lightblue', 
     xlab = substitute(paste(bar(X),"* (g)")), 
     main ="Bootstrap distribution of means \n for NC birth weights ", freq= FALSE)
abline(v = mean(Babies$Weight), col = "blue")
abline(v = mean(bsMean), col = "red")
boot.bias <- mean(bsMean) - mean(Babies$Weight)
boot.bias
ggplot(data = data.frame(bsMean = bsMean), aes(x = bsMean)) + 
  geom_density(fill = "lightblue") + 
  labs(x = substitute(paste(bar(X),"* (g)")), 
       title = "Bootstrap distribution of means for NC birth weights") + 
  theme_bw() 
quantile(bsMean, prob = c(0.025, 0.975))


## ---------------------------------------------------------------------------------------------------
Babies %>% 
  rep_sample_n(size = 1009, replace = TRUE, reps = 10000) %>% 
  group_by(replicate) %>% 
  summarize(stat = mean(Weight)) -> BSD
c(mean(BSD$stat), sd(BSD$stat))
# OR
BSD %>% 
  summarize(BS_MEAN = mean(stat), BS_SD = sd(stat))
ggplot(data = BSD, aes(x = stat)) + 
  geom_histogram(fill = "lightblue", color = "black") + 
  labs(x = substitute(paste(bar(X),"* (g)")), 
       title = "Bootstrap distribution of means for NC birth weights") + 
  theme_bw() 
quantile(BSD$stat, prob = c(0.025, 0.975))


## ---------------------------------------------------------------------------------------------------
library(infer)
bsd <- Babies %>% 
  specify(response = Weight) %>% 
  generate(reps = 10^4, type = "bootstrap") %>% 
  calculate(stat = "mean")
visualize(bsd)
get_confidence_interval(bsd, level = 0.95, type = "percentile") -> CI # Note: type = "percentile" by default
CI
# This is the same as:
quantile(bsd$stat, probs = c(0.025, 0.975))
#####################################################################
(BSB <- mean(bsd$stat) - mean(Babies$Weight))  # Bootstrap Bias
visualize(bsd) + 
  shade_confidence_interval(endpoints = CI, color = "red", fill = "purple") + 
  theme_bw()


## ----packageBOOT, fig.height=3.5, fig.width=7-------------------------------------------------------
set.seed(13)
library(boot)
bs.mean <- function(data, i){
  d <- data[i]
  M <- mean(d)
  M
  }
boot.obj <- boot(data = Babies$Weight, statistic = bs.mean, R = 10^4-1)
plot(boot.obj)
BIAS <- mean(boot.obj$t) - boot.obj$t0
BIAS
boot.obj
BCI <- boot.ci(boot.obj, type = "perc", conf = 0.92)
BCI


## ---- label = "GGP", fig.height=3.5, fig.width=7----------------------------------------------------
DF <- data.frame(x = boot.obj$t)
p1 <- ggplot(data = DF, aes(x = x)) + 
  geom_density() + 
  theme_bw() + 
  labs(x = "t*")
p2 <- ggplot(data = DF, aes(sample = x)) +
  stat_qq() + 
  theme_bw()
gridExtra::grid.arrange(p1, p2, ncol = 2)
# Or
library(patchwork)
p1 + p2

## ----COOLpic, fig.height=7, fig.width=7-------------------------------------------------------------
opar <- par(no.readonly = TRUE)
set.seed(11)
par(mfrow = c(3, 2))
curve(dnorm(x, 25, 7), from = 25 - 2.5*7, 25 + 2.5*7, col = "blue", main = "N(25, 7)", ylab = "", xlab = "")
abline(v = 25, col = "red")
curve(dnorm(x, 25, 1), from = 25 - 2.5*7, 25 + 2.5*7, col = "blue", main = "N(25, 1)", ylab = "", xlab = "")
abline(v = 25, col = "red")
rs1 <- rnorm(49, 25, 7)
rs2 <- rnorm(49, 25, 7)
hist(rs1, xlab = "", main = "n = 49")
abline(v = mean(rs1), col = "red")
B <- 10000
my.boot.stat1 <- numeric(B)
my.boot.stat2 <- numeric(B)
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
par(opar)


## ----cool2, fig.height=7, fig.width=7---------------------------------------------------------------
opar <- par(no.readonly = TRUE)
set.seed(16)
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
par(opar)


## ----ARSENIC----------------------------------------------------------------------------------------
Bang <- Bangladesh


## ----Bang2, fig.height=7, fig.width=7---------------------------------------------------------------
opar <- par(no.readonly = TRUE)
par(mfrow=c(2, 2))
Arsenic <- Bang$Arsenic
hist(Arsenic, breaks = "Scott", main = "Figure 6a", col = "lightblue")
qqnorm(Arsenic, main = "Figure 6b")
qqline(Arsenic, col = "red")
B <- 10000
n <- sum(!is.na(Arsenic))
arsenic.mean <- numeric(B)
set.seed(7)
for (i in 1:B){
  x <- sample(Arsenic, size = n, replace = TRUE)
  arsenic.mean[i] <- mean(x)
}
hist(arsenic.mean, main = "Figure 7a", col = "lightblue", breaks = "Scott", xlab = substitute(paste(bar(X),"*")))
qqnorm(arsenic.mean, main = "Figure 7b")
qqline(arsenic.mean, col = "red")
par(opar)


## ----SimpleSUM--------------------------------------------------------------------------------------
BELOW <- sum(arsenic.mean <= (mean(arsenic.mean) + qnorm(.025)*sd(arsenic.mean)))/B
ABOVE <- sum(arsenic.mean >= (mean(arsenic.mean) + qnorm(.975)*sd(arsenic.mean)))/B
c(BELOW, ABOVE)


## ---------------------------------------------------------------------------------------------------
Bang %>% 
specify(response = Arsenic) %>% 
  generate(reps = 10^4, type = "bootstrap") %>% 
  calculate(stat = "mean") -> bsd
get_confidence_interval(bsd, level = 0.95) -> CI
CI
# Or
quantile(bsd$stat, probs = c(0.025, 0.975))
visualize(bsd) + 
  shade_confidence_interval(endpoints = CI) +
  theme_bw()


## ----CI1, comment = NA------------------------------------------------------------------------------
quantile(bsMean, probs = c(0.025, 0.975))


## ----TV1--------------------------------------------------------------------------------------------
head(TV)
ct <- tapply(TV$Times, TV$Cable, mean)
ct
# Tidy approach
TV %>%
  group_by(Cable) %>%
  summarize(Means = mean(Times), n = n())


## ----TIMES1, fig.height=3.5, fig.width=7------------------------------------------------------------
times.Basic <- subset(TV, select = Times, subset = Cable == "Basic", drop = TRUE)
times.Ext <- subset(TV, select = Times, subset = Cable == "Extended", drop = TRUE)
# OR




B <- 10^4
times.diff.mean <- numeric(B)
set.seed(5)
for (i in 1:B){
  Basic.sample <- sample(times.Basic, size = sum(!is.na(times.Basic)), replace = TRUE)
  Ext.sample <- sample(times.Ext, size = sum(!is.na(times.Ext)), replace = TRUE)
  times.diff.mean[i] <- mean(Basic.sample) - mean(Ext.sample)
}
opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2))
CI <- quantile(times.diff.mean, prob = c(0.025, 0.975))
CI
hist(times.diff.mean, breaks = "Scott", freq=FALSE, 
     main = "Bootstrap Distribution \n (Figure 8a)", 
     xlab = substitute(paste(bar(x)[1],"*", - bar(x)[2],"*")), 
     col = "lightblue")
abline(v = c(0, CI), col = c("blue", "red", "red"), lwd = 2, 
       lty = c("solid", "dashed", "dashed"))
qqnorm(times.diff.mean, main = "Normal Q-Q Plot \n (Figure 8b)")
qqline(times.diff.mean, col = "red")
par(opar)

# Using infer
TV %>% 
  specify(Times ~ Cable) %>% 
  generate(reps = 10^4 - 1, type = "bootstrap") %>% 
  calculate(stat = "diff in means", order = c("Basic", "Extended")) -> bootdist
visualize(bootdist) + theme_bw() +
  labs(x = substitute(paste(bar(x)[basic],"*", - bar(x)[extended],"*")))
get_confidence_interval(bootdist, level = 0.95) -> CI2
###
visualize(bootdist) + theme_bw() +
  labs(x = substitute(paste(bar(x)[basic],"*", - bar(x)[extended],"*"))) + 
  shade_confidence_interval(endpoints = CI2) + 
  geom_vline(xintercept = 0, color = "purple", size = 2)


## ----TIMES2a, fig.height=3.5, fig.width=7-----------------------------------------------------------
obsDIFF <- mean(times.Basic) - mean(times.Ext)
P <- 10^4
times.diff.meanP <- numeric(P)
set.seed(4)
for (i in 1:P){
  index <- sample(length(TV$Times), size = length(TV$Times[TV$Cable == "Basic"]), replace = FALSE)
  times.diff.meanP[i] <- mean(TV$Times[index]) - mean(TV$Times[-index])
}
opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2))
hist(times.diff.meanP, breaks = "Scott", freq=FALSE, 
     main = "Randomization Distribution \n (Figure 9a)", 
     xlab = substitute(paste(bar(x)[basic],"*", - bar(x)[extended],"*")), 
     col = "lightblue")
abline(v= obsDIFF, col = "blue", lty = "dashed")
qqnorm(times.diff.meanP, main = "Normal Q-Q Plot \n (Figure 9b)")
qqline(times.diff.meanP, col = "red")
par(opar)
pvalue <- (sum(times.diff.meanP >= obsDIFF) + 1)/(P + 1)
pvalue


## ----Phone1, fig.height=3.5, fig.width=7------------------------------------------------------------
Phone <- Verizon
rt <- tapply(Phone$Time, Phone$Group, mean)
rt
# Tidy approach
Phone %>% 
  group_by(Group) %>% 
  summarize(Mean = mean(Time), n = n(), SD = sd(Time))
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
     main = "Bootstrap Distribution \n Figure 10a", 
     freq= FALSE, xlab = substitute(paste(bar(x),"*")))
qqnorm(ILECmean, main = "Normal Q-Q Plot \n Figure 10b")
qqline(ILECmean, col = "red")
par(opar)
CI <- quantile(ILECmean, prob = c(0.025, 0.975))
CI


## ----Timesb2, fig.height=3.5, fig.width=7-----------------------------------------------------------
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
     main = "Bootstrap Distribution \n Figure 11a", 
     freq= FALSE, xlab = substitute(paste(bar(x),"*")))
qqnorm(CLECmean, main = "Normal Q-Q Plot \n Figure 11b")
qqline(CLECmean, col = "red")
par(opar)
CIC <- quantile(CLECmean, prob = c(0.025, 0.975))
CIC


## ----Times12ab, fig.height=3.5, fig.width=7---------------------------------------------------------
B <- 10^4
diffmeans <- numeric(B)
set.seed(1)
for (i in 1:B){
  ILEC.sample <- sample(times.ILEC, size = length(times.ILEC), replace = TRUE)
  CLEC.sample <- sample(times.CLEC, size = length(times.CLEC), replace = TRUE)
  diffmeans[i] <- mean(ILEC.sample) - mean(CLEC.sample)
}
CIdiff <- quantile(diffmeans, prob = c(0.025, 0.975))
CIdiff
opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2))
hist(diffmeans, breaks = "Scott", col = "lightblue", 
     main = "Bootstrap Distribution \n Figure 12a", 
     freq= FALSE, xlab = substitute(paste(bar(x)[ILEC],"*", - bar(x)[CLEC],"*")))
abline(v = c(CIdiff, 0), col = c("blue", "blue", "red"), lwd = 2, 
       lty = c("dashed", "dashed", "solid"))
qqnorm(diffmeans, main = "Normal Q-Q Plot \n Figure 12b")
qqline(diffmeans, col = "red")
par(opar)


## ----TWOIP------------------------------------------------------------------------------------------
head(Cameras)


## ----Camera1a, fig.height=3.5, fig.width=7----------------------------------------------------------
CameraDiffs <- Cameras$JR - Cameras$BH
B <- 10^5
boot.diffs <- numeric(B)
set.seed(2)
for (i in 1:B){
  boot.diffs[i] <- mean(sample(CameraDiffs, size = length(CameraDiffs), replace = TRUE))
}
CIdiff <- quantile(boot.diffs, prob = c(0.025, 0.975))
CIdiff
opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2))
hist(CameraDiffs, freq= FALSE, col = "lightblue", 
     main = "Distribution of Price Differences \n Figure 13a", 
     xlab = "Dollars")
abline(v = mean(CameraDiffs), col = "red")
hist(boot.diffs, freq= FALSE, col = "lightblue", 
     main = "Bootstrap Distribution of \n Price Differences \n Figure 13b", 
     xlab = substitute(paste(bar(D),"*")))
abline(v = mean(boot.diffs), col = "red")
par(opar)


## ----Robust, fig.height=3.5, fig.width=7------------------------------------------------------------
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
###############
opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2))
hist(diffmeans.25, breaks = "Scott", col = "lightblue", 
     main = "Bootstrap Distribution \n Figure 14a \n 0.25 Trimmed Means", 
     freq= FALSE, xlab = substitute(paste(bar(x)[1],"*", - bar(x)[2],"*")))
abline(v = c(CIdiff.25, 0), col = c("blue", "blue", "red"), 
       lty = c("dashed", "dashed", "solid"))
qqnorm(diffmeans.25, main = "Normal Q-Q Plot \n Figure 14b")
qqline(diffmeans.25, col = "red")
par(opar)


## ----Verizon6, fig.height=3.5, fig.width=7----------------------------------------------------------
Phone <- Verizon
times.ILEC <- subset(Phone, select = Time, subset = Group == "ILEC", drop = TRUE)
times.CLEC <- subset(Phone, select = Time, subset = Group == "CLEC", drop = TRUE)
set.seed(4)
B <- 10^4
boot.ratio <- numeric(B)
for (i in 1:B){
  ILEC.sample <- sample(times.ILEC, size = length(times.ILEC), replace = TRUE)
  CLEC.sample <- sample(times.CLEC, size = length(times.CLEC), replace = TRUE)
  boot.ratio[i] <- mean(ILEC.sample)/mean(CLEC.sample)
}
CIratio <- quantile(boot.ratio, prob = c(0.025, 0.975))
CIratio
#############
opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2))
hist(boot.ratio, breaks = "Scott", col = "lightblue", 
     main = "Bootstrap Distribution \n Figure 15a", 
     freq= FALSE, 
     xlab = substitute(paste(bar(x)[ILEC],"* / ", bar(x)[CLEC],"*")))
abline(v = c(CIratio, 1), lty = c("dashed", "dashed", "solid"),
       col = c("blue", "blue", "red"))
qqnorm(boot.ratio, main = "Normal Q-Q Plot \n Figure 15b")
qqline(boot.ratio, col = "red")
par(opar)


## ----BootAgain, fig.height=3.5, fig.width=7---------------------------------------------------------
library(boot)
set.seed(6)
phoneF <- function(data, i){
  d <- data[i, ]
  m <- tapply(d$Time, d$Group, mean)
  rat <- m[2]/m[1]
  rat}
boot.obj <- boot(data = Phone, statistic = phoneF, R = 10000, strata = Phone[, 2])
plot(boot.obj)
ans <- boot.ci(boot.obj, type = "perc")
ans

