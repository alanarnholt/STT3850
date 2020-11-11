# In Class Homogeneity

DATA <- c(42, 20, 38, 33, 27, 50)
MA <- matrix(data = DATA, nrow = 2, byrow = TRUE)
dimnames(MA) <- list(Pop = c("Boys", "Girls"),
                     Candy = c("Flavor1", "Flavor2", "Flavor3"))
TMA <- as.table(MA)
TMA


library(tidyverse)
NT <- TMA %>% 
  tibble::as_tibble() %>% 
  uncount(n)
NT

ggplot(data = NT, aes(x = Pop, fill = Candy)) + 
  geom_bar(position = "fill") + 
  theme_bw()

T1 <- NT %>% 
  table()
T1

chisq.test(T1)



library(infer)

B <- 10^3 - 1
X2 <- numeric(B)
for(i in 1:B){
  TN <- xtabs(~Pop + sample(Candy), data = NT)
  X2[i] <- chisq.test(TN)$stat
}

hist(X2)

# Same approach using infer

null_dist <- NT %>% 
  specify(Pop ~ Candy) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 10^3 -1, type = "permute") %>% 
  calculate(stat = "Chisq")

visualize(null_dist)


#### Test GOF

p1 <- pexp(0.25, 1)
p2 <- pexp(0.75, 1) - pexp(0.25, 1)
p3 <- pexp(1.25, 1) - pexp(0.75, 1)
p4 <- pexp(1.25, 1, lower = FALSE)
ps <- c(p1, p2, p3, p4)
ps

256/12

B <- 10^4-1
X2 <- numeric(B)
for(i in 1:B){
  rs <- sample(1:4, 100, p = ps, replace = TRUE)
  obs <- table(rs)
  X2[i] <- chisq.test(obs, p = ps)$stat
}
hist(X2)
(pvalue <- (sum(X2 >= chi_obs) +1)/(B + 1))
