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
# P(sum =6 | die1 = 3)
fractions(sum(S$die1 == 3 & S$Sum == 6)/sum(S$die1 ==3))






# P(sum >= 6|die1 = 3)
fractions(sum(S$die1 == 3 & S$Sum >= 6)/sum(S$die1 ==3))

















S <- expand.grid(die1 = 1:20, die2 = 1:20, die3 = 1:20)
S$Sum <- apply(S, 1, sum)
S
sum(S$die1 == 10)
sum(S$Sum >= 30)
sum(S$die1 == 10 & S$Sum >= 30)
fractions(sum(S$die1 == 10 & S$Sum >= 30)/sum(S$die1 == 10))
