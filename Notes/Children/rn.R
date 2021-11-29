#
n <- 1e6
set.seed(271)
U <- runif(n)
I <- duplicated(U)
(C <- sum(I))