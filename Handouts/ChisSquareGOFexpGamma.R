set.seed(13)
stuff <- rgamma(100, 3, 6)
xbar <- mean(stuff)
VAR <- var(stuff)
c(xbar, VAR)
lambda <- xbar/VAR
alpha <- lambda*xbar
c(alpha, lambda)
qs <- qgamma(seq(0, 1, by = 0.1), alpha, lambda)
OB <- cut(stuff, breaks = qs, include.lowest = TRUE)
T1 <- xtabs(~OB)
OBS <- as.vector(T1)
OBS
EXP <- rep(10, 10)
X2obs <- sum((OBS - EXP)^2/EXP)
X2obs
pvalue <- pchisq(X2obs, 10 - 1 - 2, lower = FALSE)
pvalue
curve(dchisq(x, 7), 0, 25)
abline(v = X2obs, col = "red", lty = "dashed")
####################################################
hist(stuff, freq= FALSE, col = "peru")
d.stuff <- density(stuff)
plot(d.stuff, col = "red", lwd = 2, main = "Put Something Here")
polygon(d.stuff, col = "pink")
curve(dgamma(x, alpha, lambda), 0, 2, add = TRUE, col = "red", lty = "dashed", lwd = 2)
#####################################################
library(ggplot2)
p <- ggplot(data = data.frame(x = stuff), aes(x = x)) + geom_density(fill = "yellow") + theme_bw()
p
p + stat_function(fun = dgamma, args = list(alpha, lambda), color ="darkgreen", lwd = 2, lty = "dashed")
