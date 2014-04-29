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
  
  
#notes for Rick
#If there is a hat on a greek letter, it's a statistic

#* *Skewness* The skewness of the bootstrap ditribution does relfect the skewness of the sampling distribution.
#The first point bears emphasis.  It means that *the bootstrap is not used to get better paramter estimates* because 
#the bootstrap distributions are centered around statistics theta hat calculated from the data rather than unknown 
#population values.  Drawing thousands of bootstrap observations from the original data is not like drawing 
#observations from the underlying population, it does not create new data.

#Equation for bias
Bias_B(ThetaHat*) = E(ThetaHat*) - (ThetaHat)

site <- "http://www1.appstate.edu/~arnholta/Data/Bangladesh.csv"
Bang <- read.csv(file=url(site)) # read data into Bang

#paramters
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

#95% of the data (between 2.5% to the left and 2.5% to the right of the graph) is 95% confidence interval
#Getting z-scores
BELOW <- sum(arsenic.mean <= (mean(arsenic.mean) + qnorm(.025)*sd(arsenic.mean)))/B
ABOVE <- sum(arsenic.mean >= (mean(arsenic.mean) + qnorm(.975)*sd(arsenic.mean)))/B
c(BELOW, ABOVE)

#BOOTSTRAP FOR COMPARING TWO POPULATIONS
#Given independent samples of sizes m and n from two populations,
#Draw a resample of size m with replacement from the first sample in a separate resample of size n for the 
#second sample. Compute a statistic that compares the two groups, such as the difference between the two sample means.
#Repeat this resampling process many times say 10,000.
#Construct the bootstrap distribution of the statistic. Inspect its spread, bias, and shape.

site <- "http://www1.appstate.edu/~arnholta/Data/TV.csv"
TV <- read.csv(file=url(site)) # read data into TV
head(TV)
ct <- tapply(TV$Times, TV$Cable, mean)

#Doing a sample to compare the populations
times.Basic <- subset(TV, select = Times, subset = Cable == "Basic", drop = TRUE)
times.Ext <- subset(TV, select = Times, subset = Cable == "Extended", drop = TRUE)
B <- 10^4
times.diff.mean <- numeric(B)
set.seed(5)
for (i in 1:B){
  Basic.sample <- sample(times.Basic, size = length(times.Basic), replace = TRUE)
  Ext.sample <- sample(times.Ext, size = length(times.Ext), replace = TRUE)
  times.diff.mean[i] <- mean(Basic.sample) - mean(Ext.sample)
}
opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2))
hist(times.diff.mean, breaks = "Scott", freq=FALSE, main = "Bootstrap Distribution \n (Figure 8a)", xlab = substitute(paste(bar(x)[1],"*", - bar(x)[2],"*")), col = "lightblue")
qqnorm(times.diff.mean, main = "Normal Q-Q Plot \n (Figure 8b)")
qqline(times.diff.mean, col = "red")

#getting the quantile
par(opar)
CI <- quantile(times.diff.mean, prob = c(0.025, 0.975))
CI


#NOT FINISHED
#THE BACK TWO ROWS PRESENT THIS ON TUESDAY
14. Import the data from Girls2004 (see Section 1.2).

(a) Perform some exploratory data analysis and obtain summary statistics on the
weights of baby girls born in Wyoming and Arkansas (do separate analyses for each state).

site <- "http://www1.appstate.edu/~arnholta/Data/Girls2004.csv"
girls <- read.csv(file=url(site))
hist(girls$Weight, breaks = "Scott", main = "Figure 6a", col = "lightblue")
qqnorm(girls$Weight, main = "Figure 6b")
qqline(Arsenic, col = "red")


(b) Bootstrap the difference in means, plot the distribution, and give the summary
statistics. Obtain a 95% bootstrap percentile confidence interval and
interpret this interval.

weightAK <- subset(girls, select = Weight, subset = State == "AK", drop = TRUE)
weightWY <- subset(girls, select = Weight, subset = State == "WY", drop = TRUE)
B <- 10^4
weight.diff.mean <- numeric(B)
set.seed(5)
for (i in 1:B){
  AK.sample <- sample(weightAK, size = length(weightAK), replace = TRUE)
  WY.sample <- sample(weightWY, size = length(weightWY), replace = TRUE)
  weight.diff.mean[i] <- mean(AK.sample) - mean(WY.sample)
}
opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2))
hist(weight.diff.mean, breaks = "Scott", freq=FALSE, main = "Bootstrap Distribution \n (Figure 8a)", xlab = substitute(paste(bar(x)[1],"*", - bar(x)[2],"*")), col = "lightblue")
qqnorm(weight.diff.mean, main = "Normal Q-Q Plot \n (Figure 8b)")
qqline(weight.diff.mean, col = "red")

par(opar)
CI <- quantile(weight.diff.mean, prob = c(0.025, 0.975))
CI

(c) What is the bootstrap estimate of the bias? What fraction of the bootstrap
standard error does it represent?

bootstrapEstimate = mean(weight.diff.mean) - (mean(weightAK) - mean(weightWY))

(d) Conduct a permutation test to calculate the difference in mean weights and
state your conclusion.

(e) For what population(s), if any, does this conclusion hold? Explain.
  
qqnorm(height)
qqline(height)
xbar <- mean(height)
z <- qnorm(.95)
sigma <- 2
n<-length(height)
CI <- xbar + c(-1,1)*z*sigma/sqrt(n)
CI
library(PASWR)
z.test(x=height, sigma.x = 2, conf.level = .90)
zobs <- (mean(height) - 0)/(2/sqrt(12))

s <- sd(height)
CT <- qt(.95, n-1)
CI <- xbar + c(-1, 1)*CT*s/sqrt(n)
t.test(height, mu =74, alternative ="less")


If x1 ... xn ~iid N(miu1, sigma1)
and y1 .... ym ~iid N(miu2, sigma2)
xbar - ybar ~ N(miu1-miu2, sqrt(sigma1^2/n + sigma2^2/m))
z = (xbar - ybar)-(miu1 - miu2) / sqrt(sigma1^2/n + sigma2^2/m)

site <- "http://www1.appstate.edu/~arnholta/Data/FlightDelays.csv"
FD <- read.csv(site)
library(ggplot2)
ggplot(data=FD, aes(x=Delay)) + geom_density() + facet_grid(Carrier~.)
tapply(FD$Delay, FD$Carrier, mean)
CI.93(miu_aa - miu_ua) = [-8.67, -3.096]
t.test(Delay~Carrier, data=FD, conf.level=0.93)
#df(r) = 1843.762
qt(c(.035,.965),1843.762)

Stuff <- 10^4
pTest <- numeric(Stuff)
set.seed(13)
n <- sum(!is.na(FD$Delay))
for (i in 1:Stuff){
  x <- sample(FD$Delay, size = n, replace = TRUE)
  pTest[i] <- mean(x)
}
t.test(Delay~Month, data=FD, conf.level=0.97)
