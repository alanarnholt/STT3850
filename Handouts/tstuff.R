site <- "http://www1.appstate.edu/~arnholta/Data/FlightDelays.csv"
FD <- read.csv(site)
head(FD)
library(ggplot2)
ggplot(data = FD, aes(x = Carrier, y = Delay)) + geom_boxplot() + theme_bw()
ggplot(data = FD, aes(x = Delay)) + facet_grid(Carrier ~ .) + geom_density(fill = "pink") + theme_bw()
ggplot(data = FD, aes(sample = Delay, color = Carrier)) + stat_qq() + theme_bw()
SD <- with(data = FD, tapply(Delay, Carrier, sd))
SD
MEANS <- with(data = FD, tapply(Delay, Carrier, mean))
MEANS
xtabs(~Carrier, data = FD)
#  
# Permutation test
delays <- FD$Delay
theta.obs <- MEANS[1] - MEANS[2]
names(theta.obs) <- NULL
theta.obs
SIMS <- 10^4 - 1
theta.hat <- numeric(SIMS)
set.seed(1)
for(i in 1:SIMS){
  index <- sample(2906 + 1123, size = 2906, replace = FALSE)
  theta.hat[i] <- mean(delays[index]) - mean(delays[-index])
}
hist(theta.hat, freq= FALSE, breaks = "Scott")
theta.obs
pvalue <- (sum(theta.hat <= theta.obs) + 1)/(SIMS + 1)
pvalue

set.seed(2)
for(i in 1:SIMS){
  index <- sample(2906 + 1123, size = 2906, replace = FALSE)
  theta.hat[i] <- (mean(delays[index]) - mean(delays[-index]))/sqrt((sd(delays[index])^2/2906 + sd(delays[-index])^2/1123))
}
hist(theta.hat, freq= FALSE, breaks = "Scott", xlim = c(-5, 5))
curve(dt(x, 1843), -5, 5, add = TRUE)
t.test(Delay ~ Carrier, data = FD)$stat
simPvalue <- (sum(theta.hat <= t.test(Delay ~ Carrier, data = FD)$stat) + 1)/(SIMS + 1)
simPvalue
t.test(Delay ~ Carrier, data = FD)
########################################
### Bootstrap T
########################################
B <- 10^4 - 1
naa <- 2906
nua <- 1123
Saa <- SD[1]
Sua <- SD[2]
Maa <- MEANS[1]
Mua <- MEANS[2]
Daa <- FD$Delay[FD$Carrier == "AA"]
Dua <- FD$Delay[FD$Carrier == "UA"]
SE <- sqrt(var(Daa)/naa + var(Dua)/nua)
thetahat <- mean(Daa) - mean(Dua)
Tstar <- numeric(B)
for(i in 1:B){
  bootaa <- sample(Daa, size = naa, replace = TRUE)
  bootua <- sample(Dua, size = nua, replace = TRUE)
  Tstar[i] <- (mean(bootaa) - mean(bootua) - thetahat) / sqrt(var(bootaa)/naa + var(bootua)/nua)
}
CTSTAR <- quantile(Tstar, probs = c(0.975, 0.025))
CTSTAR
CI <- thetahat - CTSTAR*SE
CI
#####################################################################################
### Using boot
library(boot)
fbt <- function(data, i){
  d <- data[i, ]
  M <- tapply(d$Delay, d$Carrier, mean)
  V <- tapply(d$Delay, d$Carrier, var)/tapply(d$Delay, d$Carrier, length)
  c(M[1] - M[2], V[1] + V[2])
}
set.seed(1)
b.obj <- boot(FD, statistic = fbt, R = B)
boot.ci(b.obj, type = c("perc", "stud"))
plot(b.obj)

####
## Prob 26
####
site <- "http://www1.appstate.edu/~arnholta/Data/MnGroundwater.csv"
GW <- read.csv(site)
str(GW)
# Base histogram
hist(GW$Alkalinity, xlab = "Alkalinity", freq = FALSE, 
     breaks = "Scott", col = "pink", main = "")
qqnorm(GW$Alkalinity)
qqline(GW$Alkalinity)
# That was ugly :( --- this is better! :)
library(ggplot2)
p <- ggplot(data = GW, aes(x = Alkalinity)) + 
  geom_density(fill = "pink", alpha = 0.3) + 
  theme_bw()
p
pqq <- ggplot(data = GW, aes(sample = Alkalinity)) + stat_qq() + theme_bw()
pqq
# part b
xbar <- mean(GW$Alkalinity)
S <- sd(GW$Alkalinity)
n <- sum(!is.na(GW$Alkalinity))
ct <- qt(c(0.975, 0.025), n - 1)
c(xbar, S, n, ct)
CI <- xbar - ct*S/sqrt(n)
CI
# using t.test
t.test(GW$Alkalinity)$conf
# 
B <- 10^4 - 1
alk <- GW$Alkalinity
MALK <- numeric(B)
TALK <- numeric(B)
set.seed(1)
for(i in 1:B){
  bootsample <- sample(alk, size = n, replace = TRUE)
  MALK[i] <- mean(bootsample)
  TALK[i] <- (mean(bootsample) - xbar)/(sd(bootsample)/sqrt(n))
}
percentileCI <- quantile(MALK, probs = c(0.025, 0.975))
percentileCI
CT <- quantile(TALK, probs = c(0.975, 0.025))
CT
CIbootT <- xbar - CT*S/sqrt(n)
CIbootT
#### Using boot
library(boot)
mb <- function(data, i){
    d <- data[i, ]
    M <- mean(d$Alkalinity)
    V <- var(d$Alkalinity)/length(d$Alkalinity)
    c(M, V)
  }
b.obj <- boot(GW, statistic = mb, R = B)
b.obj
boot.ci(b.obj, type = c("perc", "stud"))
#### Show all next to each other...
t.test(GW$Alkalinity)$conf
percentileCI
CIbootT
boot.ci(b.obj, type = c("perc", "stud"))
#### Prob 27
site <- "http://www1.appstate.edu/~arnholta/Data/TXBirths2004.csv"
Births <- read.csv(site)
head(Births)
T1 <- xtabs(~Smoker, data = Births)
T1
# b
p <- ggplot(data = Births, aes(x = Weight)) + 
  geom_density(fill = "blue", alpha = 0.3) + 
  facet_grid(Smoker~.) + 
  theme_bw()
p
# c
t.test(Weight ~ Smoker, data = Births)$conf

tmf <- function(data, i){
  d <- data[i, ]
  M <- tapply(d$Weight, d$Smoker, mean)
  V <- tapply(d$Weight, d$Smoker, var)/tapply(d$Weight, d$Smoker, length)
  c(M[1] - M[2], V[1] + V[2])
}
b.obj <- boot(Births, statistic = tmf, R = 10^4 - 1)
b.obj
boot.ci(b.obj, type = c("perc", "stud"))
t.test(Weight ~ Smoker, data = Births)$conf
# part d
t.test(Weight ~ Smoker, data = Births, alternative = "greater")
