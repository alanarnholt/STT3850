#
set.seed(123)
mar <- rnorm(20, 42, 10)
apr <- rnorm(18, 52, 10)
t.test(apr, mar) -> results
results
names(results)
results$parameter -> df
df
(pt_est <- mean(apr) - mean(mar))
(CT <- qt(.975, df))
(SE <- sqrt(var(apr)/18 + var(mar)/20))
(ME <- CT*SE)
(CI <- pt_est + c(-1, 1)*ME)