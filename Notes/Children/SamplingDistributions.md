
Sampling Distributions (Chapter 4)
========================================================

A **sampling distribution** is the distribution of a *statistic* is its probability distribution.  In other words, the sampling distribution of a statistic summarizes a data set, and represents how the statistic varies across random data sets.

The standard deviation of a statistic *T* is written $\sigma_{T} = SE[T]$.  An estimate for the standard deviation of a statistic *T* is written $\hat{\sigma}_{T} = \widehat{SE[T]}$.

**EXAMPLE:** Toss a fair coin $n = 10$ times and note the proportion of heads $\hat{p}$.  If you repeat the experiment, you probably would not get the same proportion of heads.  If you toss 50 sets of 10 coin flips, you might see outcomes (i.e., proportions of heads, $\hat{p}$) such as those below.


```r
set.seed(12)
cointoss <- rbinom(50, 10, 1/2)
phat <- cointoss/10
xtabs(~phat)
```

```
## phat
## 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 
##   1   3   6  10  12  11   6   1
```

```r
plot(xtabs(~phat),ylab= "count",xlab = expression(hat(p)))
```

![plot of chunk sampDIST](figure/sampDIST.png) 

Although proportions between 0.3 and 0.7 occur most often, we see there is a proportion as low as 0.1 heads and as high as 0.8 heads.  Instead of 50 sets of 10 coin flips, the next simulation performs 50,000 sets of 10 tosses.


```r
set.seed(12)
cointoss <- rbinom(50000, 10, 1/2)
phat <- cointoss/10
xtabs(~phat)
```

```
## phat
##     0   0.1   0.2   0.3   0.4   0.5   0.6   0.7   0.8   0.9     1 
##    47   480  2187  5837 10252 12238 10321  5853  2264   469    52
```

```r
mean(phat)
```

```
## [1] 0.5006
```

```r
sd(phat)
```

```
## [1] 0.1581
```

```r
plot(xtabs(~phat),ylab= "count",xlab = expression(hat(p)))
```

![plot of chunk sampDISTsim](figure/sampDISTsim.png) 


The previous plot is an approximation to the *sampling distribution of $\hat{p}$*.  The mean of the sampling distribution of $\hat{p}$ is $\mu_{\hat{p}} = p$, and the standard deviation of the sampling distribution of $\hat{p}$, $\sigma_{\hat{p}} = \sqrt{p\times(1-p)/n}$.  The standard deviation of the simulated $\hat{p}$ values, the standard deviation of a statistic, is called a *standard error*.  In this simulation, the estimated mean of the statistic is written as $\hat{\mu}_{\hat{p}}=$ 0.5006. The standard error of $\hat{p}$, sometimes written as $SE(\hat{p})$ or $\sigma_{\hat{p}}$ is 0.1581.

## Probability Review

Let $X:S \rightarrow \textbf{R}$ denote a random variable and $f$ denote its density function.  The mean of $X$, also known as the *expected value* of $X$, is

$$E[X] = \mu_X = \mu = \int_{-\infty}^{\infty}xf(x)\,dx$$
and the variance of $X$ is
$$V[X]=\sigma^2_{X}=\int_{-\infty}^{\infty}(x - \mu_{X})^2f(x)\,dx.$$

If $X:S\rightarrow {x_1, x_2, \ldots}$ is a discrete random variable with $P(X = x_i)=p_i$, then

$$E[X] = \mu_X = \mu = \sum_{i=1}x_i\cdot p_i$$
and the variance of $X$ is
$$V[X]=\sigma^2_{X}=\sum_{i = 1}(x_i - \mu_X)^2\cdot p_i.$$

If $X_1, X_2, \ldots , X_n$ are identically distributed random variables with mean $\mu$ and variance $\sigma^2$, then

$$E[\bar{X}] = \mu_{\bar{X}} = \mu,$$ and
$$Var[\bar{X}] = \sigma^2_{\bar{X}} = \frac{\sigma^2}{n}$$

If $X$ and $Y$ are random variables, then

$$E[X + Y] = E[X] + E[Y],$$ and if X and Y are **independent** then
$$V[X + Y] = V[X] + V[Y].$$

Given constants $a$ and $b$ and random variables $X$ and $Y$,

$$E[aX \pm bY] = aE[X] \pm bE[Y],$$
and if $X$ and $Y$ are independent then
$$V[aX \pm bY] = a^2V[X] + b^2V[Y].$$

**Theorem:** Let $X_1, X_2, \ldots, X_n$ be independent normal random variables with mean $\mu_i$,
and variance $\sigma^2_i, i = 1, 2, \ldots, n,$ respectively. Let $a_1, a_2, \ldots, a_n$ be arbitrary constants.  Then $a_1X_1 + a_2X_2 + \cdots + a_nX_n$ is a normal variable with mean $a_1\mu_1 + a_2\mu_2 + \cdots a_n\mu_n$ and variance $a_1^2\sigma_1^2 + a_2^2\sigma_2^2 + \cdots + a_n^2\sigma_n^2.$

**Corollary:**  Let $X_1, X_2, \ldots, X_n$ be independent normal random variables 
with common mean $\mu$, and common variance $\sigma^2$.  Then 
$$ \bar{X} \sim N(\mu_\bar{X}, \sigma_{\bar{X}})$$

Recall that $\mu_{\bar{X}} = \mu$ and that $\sigma_{\bar{X}} = \frac{\sigma}{\sqrt{n}}$, so the previous result is often written as 

$$ \bar{X} \sim N(\mu, \sigma/\sqrt{n})$$

**Do problem 13 now.**

## Calculating Sampling Distributions

**Theorem 4.1** Suppose we have continuous random variables $X_1, X_2, \ldots, X_n$ that are i.i.d with pdf $f$ and cdf $F$.  Define their minimum and maximum to be the random variables

$$X_{\textrm{min}} = \textrm{min}\{X_1, X_2, \ldots, X_n\},$$
$$X_{\textrm{max}} = \textrm{max}\{X_1, X_2, \ldots, X_n\}.$$

Then the pdf's for $X_{\textrm{min}}$ and $X_{\textrm{max}}$ are

$$f_{\textrm{min}}(x) = n\left(1 - F(x)\right)^{n-1}f(x),$$
$$f_{\textrm{max}}(x) = nF^{n-1}(x)f(x).$$

**Example 4.5**  Let $X_1, X_2, \ldots, X_{10}$ be a random sample from a distribution with pdf
$f(x)  = 2/x^3, x \geq1.$  Let $X_{\textrm{min}}$ denote the minimum of the sample.  Find the probability that $X_{\textrm{min}}$ is less than or equal to 1.2.

**Solution:**  First, we compute the cdf $F$ corresponding to $f$:
$$F(x) = \int_{1}^{x}\frac{2}{t^3}\,dt = \frac{-1}{t^2}\Bigr|_{1}^{x} = 1 - \frac{1}{x^2}$$

If follows that

$$f_{\textrm{min}}(x) = 10\left(1 - \left(1 - \frac{1}{x^2}\right)\right)^{10-1}\frac{2}{x^3} =  \frac{20}{x^{21}}, x \geq 1.$$

Therefore,

$$P(X_{\textrm{min}} \leq 1.2) = \int_{1}^{1.2}\frac{20}{x^{21}}\,dx = \frac{-1}{x^{20}}\Bigr|_{1}^{1.2} = 1 - \frac{1}{1.2^{20}} = 0.9739$$.

One might also choose to integrate numerically using the function `integrate()`.


```r
fmin <- function(x){20/x^21}
integrate(fmin, 1, 1.2)
```

```
## 0.9739 with absolute error < 1.1e-14
```


## The Central Limit Theorem

Let $X_1, X_2,...,X_n$ be independent identically distributed random variables with mean $\mu$ and variance $\sigma^2$, both finite.  Then for any constant z,

$$\lim_{n \to \infty} P\left(\frac{\bar{X} - \mu}{\sigma/\sqrt{n}} \leq z \right) = \Phi(z) $$

Where $\Phi$ is the cdf of the standard normal distribution.  The central limit theorem means that for ``sufficiently large'' $n$, the sampling distribution of $\bar{X}$ is approximately normal with mean $\mu_{\bar{X}}= \mu$, and standard deviation $\sigma_{\bar{X}} = \sigma/\sqrt{n}$.  Recall that the standard deviation of a statistic is called a standard error.  Consequently, our text refers to $\sigma_{\bar{X}}$ as $SE(\bar{X})$.

#### Note:  If you one is sampling from a normal distribution, then the resulting sampling distribution of the sample mean is exactly normal.  It is always the case that $\mu_{\bar{X}} = \mu$ and $\sigma_{\bar{X}} = \sigma/\sqrt{n}$ regardless of the population one is sampling from.

## Just how large is ''sufficiently large''?

Consider the following simulations


```r
set.seed(123)
par(mfrow=c(4,3))
# X~N(50,15)
x <- seq(0,100,.01)
y <- dnorm(x,50,15)
plot(x,y,type="l",col="blue",lwd=2,main="X~N(50,15)",xlab="",ylab="")

# X~U(0,1)
x <- seq(0,1,.001)
y <- dunif(x,0,1)
plot(x,y,type="l",col="blue",lwd=2,main="X~U(0,1)",xlab="",ylab="")

# X~Exp(1)
x <- seq(0,5,.01)
y <- dexp(x,1)
plot(x,y,type="l",col="blue",lwd=2,main="X~Exp(1)",xlab="",ylab="")

m <- 20000  # Number of Samples
EX <- 1.2   # Character expansion

xbar.5 <- apply(matrix(rnorm(m*5,50,15),nrow=m),1,mean)
hist(xbar.5,breaks="Scott",col="blue",xlim=c(0,100),prob=T,xlab="",ylab="",main="")
mtext(  expression(bar(x)[5]),side=3,line=1,cex=EX)

xbar.5 <- apply(matrix(runif(m*5,0,1),nrow=m),1,mean)
hist(xbar.5,breaks="Scott",col="blue",xlim=c(0,1),prob=T,xlab="",ylab="",main="")
mtext(  expression(bar(x)[5]),side=3,line=1,cex=EX)

xbar.5 <- apply(matrix(rexp(m*5,1),nrow=m),1,mean)
hist(xbar.5,breaks="Scott",col="blue",xlim=c(0,5),prob=T,xlab="",ylab="",main="")
mtext(  expression(bar(x)[5]),side=3,line=1,cex=EX)

xbar.10 <- apply(matrix(rnorm(m*10,50,15),nrow=m),1,mean)
hist(xbar.10,breaks="Scott",col="blue",xlim=c(0,100),prob=T,xlab="",ylab="",main="")
mtext(  expression(bar(x)[10]),side=3,line=1,cex=EX)

xbar.10 <- apply(matrix(runif(m*10,0,1),nrow=m),1,mean)
hist(xbar.10,breaks="Scott",col="blue",xlim=c(0,1),prob=T,xlab="",ylab="",main="")
mtext(  expression(bar(x)[10]),side=3,line=1,cex=EX)

xbar.10 <- apply(matrix(rexp(m*10,1),nrow=m),1,mean)
hist(xbar.10,breaks="Scott",col="blue",xlim=c(0,5),prob=T,xlab="",ylab="",main="")
mtext(  expression(bar(x)[10]),side=3,line=1,cex=EX)

xbar.30 <- apply(matrix(rnorm(m*30,50,15),nrow=m),1,mean)
hist(xbar.30,breaks="Scott",col="blue",xlim=c(0,100),prob=T,xlab="",ylab="",main="")
mtext(  expression(bar(x)[30]),side=3,line=1,cex=EX)

xbar.30 <- apply(matrix(runif(m*30,0,1),nrow=m),1,mean)
hist(xbar.30,breaks="Scott",col="blue",xlim=c(0,1),prob=T,xlab="",ylab="",main="")
mtext(  expression(bar(x)[30]),side=3,line=1,cex=EX)

xbar.30 <- apply(matrix(rexp(m*30,1),nrow=m),1,mean)
hist(xbar.30,breaks="Scott",col="blue",xlim=c(0,5),prob=T,xlab="",ylab="",main="")
mtext(  expression(bar(x)[30]),side=3,line=1,cex=EX)
```

![plot of chunk CLTsim](figure/CLTsim.png) 

```r

par(mfrow=c(1,1))
```


#### Note:  The usual rule of thumb, found in most textbooks, is that the CLT is reasonably accurate if $n \geq 30$.  Such rules are wishful thinking, dating to a pre-computer age when one had few realistic alternatives to using the CLT because most other methods were computationally infeasible.


## CLT for Binomial Data

**Example 4.9** Toss a fair coin 300 times.  Find the approximate probability of getting at most 160 heads.

**Solution:** Let $X=$ the number of heads in 300 flips of a fair coin.  Since $X \sim Bin(n = 300, p = 1/2)$ it follows that $\mu_X = np$ and $\sigma_{X} =\sqrt{np\times(1 - p)}$, and $Y \dot{\sim} N(np, \sqrt{np\times(1 - p)})$.  It follows that $P(X \leq 160) = P\left(\frac{X - np}{\sqrt{np \times (1 - p)}} = Z \leq \frac{160 - 150}{\sqrt{300/4}}\right).$  Furthermore, $P(Z \leq 1.154701)=$ 0.8759.  Solve the problem in class using $\hat{p}$ as the random variable.

**Exact Solution:** $P(X \leq 160)= \sum_{x = 0}^{x = 160}\binom{300}{x} 0.5^x 0.5^{300-x}=$ 0.8874.


```r
pbinom(160, 300, 1/2)
```

```
## [1] 0.8874
```

