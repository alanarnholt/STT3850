# Notes 2/28/2024
S <- expand.grid(die1 = 1:6, die2 = 1:6)
S$Sum <- apply(S, 1, sum)
S








S$die1 == 3
S$Sum == 6
S$die1 == 3 & S$Sum == 6
sum(S$die1 == 3)
sum(S$Sum == 6)
sum(S$die1 == 3 & S$Sum == 6)



library(MASS)
fractions(sum(S$die1 == 3 & S$Sum == 6)/sum(S$die1 ==3))



















S <- expand.grid(die1 = 1:20, die2 = 1:20, die3 = 1:20)
S$Sum <- apply(S, 1, sum)
S
sum(S$die1 == 12)
fractions(sum(S$die1 ==12 & S$Sum == 46)/sum(S$die1 == 12))
