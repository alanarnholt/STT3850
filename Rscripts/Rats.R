Worms <- c(17, 16, 10, 10, 7, 10, 7, 2, 2, 1)
Group <- c(rep("Control", 5), rep("Treatment", 5))
DF <- data.frame(Worms, Group)
DF








tapply(DF$Worms, DF$Group, mean)
diff(tapply(DF$Worms, DF$Group, mean))




B <- 10^4
md <- numeric(B)
for(i in 1:B){
  md[i] <- diff(tapply(DF$Worms, sample(DF$Group), mean))
}


B <- 10^4
md <- numeric(B)
for(i in 1:B){
  index <- sample(1:10, 5, replace = FALSE)
  md[i] <- mean(Worms[index]) - mean(Worms[-index])
}
hist(md)
  