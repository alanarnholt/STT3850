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
