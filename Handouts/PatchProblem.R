## Patch Data
library(bootstrap)
## read about patch --- use ?patch
patch
##
thetaHat <- mean(patch$y)/mean(patch$z)
thetaHat
n <- dim(patch)[1]
n
B <- 10^4 - 1
ths <- numeric(B)
set.seed(13)
for(b in 1:B){
  i <- sample(1:n, size = n, replace = TRUE)
  y <- patch$y[i]
  z <- patch$z[i]
  ths[b] <- mean(y)/mean(z)
}
sd(ths)
mean(ths)
###########
n <- dim(patch)[1]
n
B <- 10^4 - 1
ths <- numeric(B)
set.seed(13)
for(b in 1:B){
  d <- patch[, c("z","y")]
  i <- sample(1:n, size = n, replace = TRUE)
  ds <- d[i, ]
  ths[b] <- mean(ds$y)/mean(ds$z)
}
sd(ths)
mean(ths)
quantile(ths, probs = c(0.05, 0.95))
BootBias <- mean(ths) - thetaHat
BootBias
BootBias/sd(ths)
################
## Now use boot package
##
library(boot)
stat <- function(data, i){
  d <- data[i, ]
  ths <- mean(d$y)/mean(d$z)
  thsV <- var(d$y)/var(d$z)  # or is it var((d$y)/(d$z)) ???
  c(ths, thsV)
}
set.seed(13)
b.obj <- boot(patch, statistic = stat, R = B)
boot.ci(b.obj, type = "perc")
boot.ci(b.obj)
boot.ci(b.obj, type = "stud", conf = 0.95)

