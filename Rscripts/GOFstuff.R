# Generated some data
set.seed(131)
# grades <- rnorm(50, 75, 8)
grades <- runif(50, 51, 99)
hist(grades)
# Estimate mean mu and sd sigma
xbar <- mean(grades)
SD <- sd(grades)
c(xbar, SD)
#
LG <- cut(grades, c(-Inf, qnorm(.10, xbar, SD), qnorm(.25, xbar, SD), qnorm(.75, xbar, SD), 
                    qnorm(.90, xbar, SD), Inf), labels = c("F", "D", "C", "B", "A"))
T1 <- table(LG)
T1

chisq.test(T1, p = c(.10, .15, .50, .15, .1))$obs
chisq.test(T1, p = c(.10, .15, .50, .15, .1))$exp
chisq.test(T1, p = c(.10, .15, .50, .15, .1))$stat -> obs_stat
pvalue <- pchisq(obs_stat, 2, lower = FALSE)
pvalue













##########################################
NG <- read.csv("NumericGrades.csv")
summary(NG)
hist(NG$NumericGrade)
xbar <- mean(NG$NumericGrade)
SD <- sd(NG$NumericGrade)
c(xbar, SD)
NG2 <- NG$NumericGrade[NG$NumericGrade >= 40]
xbar2 <- mean(NG2)
SD2 <- sd(NG2)
c(xbar2, SD2)
#############################################
LG <- cut(NG2, c(-Inf, qnorm(.10, xbar2, SD2), qnorm(.25, xbar2, SD2), 
                    qnorm(.75, xbar2, SD2), 
                    qnorm(.90, xbar2, SD2), Inf), labels = c("F", "D", "C", "B", "A"))
T3 <- table(LG)
T3

chisq.test(T3, p = c(.10, .15, .50, .15, .1))$obs
chisq.test(T3, p = c(.10, .15, .50, .15, .1))$exp
chisq.test(T3, p = c(.10, .15, .50, .15, .1))$stat -> obs_stat
pvalue <- pchisq(obs_stat, 2, lower = FALSE)
pvalue
