# Alan
# set.seed(352)
set.seed(352)
rv <- rt(100, 2)
p1 <- pnorm(-1)
p2 <- pnorm(0) - pnorm(-1)
p3 <- pnorm(1) - pnorm(0)
p4 <- pnorm(1, lower = FALSE)
ps <- c(p1, p2, p3, p4)
ps
sum(rv >= 1)
obs <- c(22, 26, 29, 23)
chisq.test(obs, p = ps)
