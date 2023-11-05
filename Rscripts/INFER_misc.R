library(infer)
vignette("infer")
vignette("observed_stat_examples")


library(resampledata)
set.seed(321)
Arsenic <- subset(Bangladesh, select = Arsenic, drop = T)
xbar <- mean(Arsenic)
S <- sd(Arsenic)
N <- 10^4
n <- length(Arsenic)
Tstar <- numeric(N)
Sstar <- numeric(N)
Xbarstar <- numeric(N)
set.seed(13)
for (i in 1:N)
{
  x <-sample(Arsenic, size = n, replace = T)
  Xbarstar[i] <- mean(x)
  Sstar[i] <- sd(x)
}
Tstar <- (Xbarstar - xbar)/(Sstar / sqrt(n))
CIt <- quantile(Tstar, c(0.025, 0.975))
names(CIt) <- NULL
CIt
(BPCI <- quantile(Xbarstar, probs = c(0.025, 0.975)))



##################################################

Bangladesh %>%
  specify(response = Arsenic) %>%
  hypothesize(null = "point", mu = 125.3199) %>%
  generate(reps = 10^3, type = "bootstrap") %>% 
  calculate(stat = "t") -> bt
bt
visualize(bt)
get_ci(bt, level = .90) -> Q
Q
BCI <- c(xbar -Q[2]*S/sqrt(n), xbar - Q[1]*S/sqrt(n))
BCI


