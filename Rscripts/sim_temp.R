#
set.seed(123)
mar <- rnorm(20, 42, 10)
apr <- rnorm(18, 52, 10)
t.test(apr, mar) -> results
results
names(results)
results$parameter -> df
df
(nu <- (var(apr)/18 + var(mar)/20)^2 / ((var(apr)/18)^2/(18-1) + (var(mar)/20)^2/(20-1)))
(pt_est <- mean(apr) - mean(mar))
(CT <- qt(.975, df))
(SE <- sqrt(var(apr)/18 + var(mar)/20))
(ME <- CT*SE)
(CI <- pt_est + c(-1, 1)*ME)
results$conf.int
#
library(PASWR2)
z.test(apr, mar, sigma.x = 10, sigma.y = 10) -> resZ
resZ
resZ$conf.int
(CZ <- qnorm(.975))
(SD <- sqrt(10^2/18 + 10^2/20))
(ME <- CZ*SD)
(CIZ <- pt_est + c(-1, 1)*ME)
resZ$conf.int