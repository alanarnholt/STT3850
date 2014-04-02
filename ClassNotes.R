#Notes
#Parameters  Statistics
#p           p-hat
#mu          y-bar
#sigma       s
#sigma^2     s^2
#Ho: p1 - p2 = 0



#Always add Variance!

#Variance and Standard Deviation 
#V[X] = E[X^2] - E[X]^2
#V[X] = sigma^2X = [0-3/2]^2 * 1/8 + [1-3/2]^2 * 3/8 + [2-3/2]^2 * 1/8 + [3-3/2]^2 * 1/8 = 3/4

#1) Let X1, X2, ... X9 ~ N(20,9), Y1, Y2, ...., Y16 ~ N(16,4), and Z1, Z2 ... Z25 ~ N(16,5). 
#LetW1 = xbar + ybar, W2=4X-2Y, W3 = 2xbar - 3ybar + zbar

#E[X] = miu

#E[w1] = E[xbar] + E[ybar]

#V[xbar+ybar] = V[xbar] + V[ybar]
#V[X]/N + V[Y]/N

#a) What is the exact distribution of W1?
Expected value of W1 -> E(W1) = 36
Variance of W1 -> V(W1) = 10

#b) fancyP(W1 < 33) = ______    pnorm(33, 36, sqrt(10))
.1714

#c) Give the exact distribution of W2.
#E[W2] = 4*X - 2*Y
#V[w2] = v[4X - 2Y] = 4^2 * V[X] + 2^2 * V[Y]

Expected value of W2 -> E(W2) = 48
Variance of W2 -> V(W2) = 1360

#d) fancyP(0 < W2 < 64) = ________   pnorm(64, 48, sqrt(1360)) - pnorm(0, 48, sqrt(1360))
0.5712757

#e) What is the exact distribution of W3?
Expected value of W3 -> E(W3) = 8
Variance of W3 -> V(W3) = 46

#f) fancyP(W3 <= q) = 0.05 => q = ?
qnorm(0.05, 8, sqrt(46)) = -3.15594


#2) A friend claims that she has drawn a random sample of size 24 from a uniform distribution with a=0, b=6.

#a) What is the expected value of the sample mean? 
E[X] = 3

#b) What is the variance of the sample mean?


#c)What is the standar deviation of the sample mean?

set.seed(13)
samples <- 10000
xbar <- numeric(samples)
ybar <- numeric(samples)
for(i in 1:samples){
  
}

W1 <- xbar + ybar
mean(W1)
var(W1)
sd(W1)
hist(W1, breaks = "scott", col = "pink")
library(ggplot2)
ggplot(data = data.frame(x=W1), aes(x=x)) + geom_density(fill = "pink") + theme_bw()

f<- function(x){3/8 * x^2}
curve(f,0,2, col = "red")
integrate(f, 0, 1/5)$value

latek
$\int_{0}^{2}\frac{3}{8}y^2, \dy = `r ans`$
  
  #anything with a bar is sample mean
  #
  