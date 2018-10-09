# Alan Arnholt

p <- 0.5
n <- 10
x <- 0:10
px <- dbinom(x, n, p)
plot(x, px, type = "h")
curve(dnorm(x, n*p, sqrt(n*p*(1 - p))), add = TRUE)

EX <- sum(x*px)
VX <- sum((x - EX)^2*px)
c(EX, VX)

sims <- 10000
phat <- numeric(sims)
for(i in 1:sims){
x <- rbinom(1, n, 0.5)
phat[i] <- x/n
}
mean(phat)
sd(phat)
hist(phat)
table(phat)
barplot(table(phat))
barplot(table(phat)/sims)

# Theoretical
SEphat <- sqrt(p*(1 - p)/n)
SEphat

# phat ~ N(p, sqrt(p*(1-p)/n))

# P(X >= 23.3) = 1 - P(X <= 23)----Actual answer
pbinom(23, 233, .07, lower = FALSE)
# Approximate answer
#P(phat >= .10)
pnorm(.10, .07, sqrt(.07*(1-.07)/233), lower = FALSE)
