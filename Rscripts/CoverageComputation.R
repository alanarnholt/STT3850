alpha <-0.05 
n<-30 #number of trials 
x <-0:n 
sp <-x/n #sample proportion 
m.err <-qnorm(1-alpha/2)*sqrt(sp*(1-sp)/n) 
lcl <-sp-m.err  
ucl <-sp + m.err 
pp <- 0.79 #pp =P(Success) 
prob <-dbinom(x,n,pp) 
cover <-(pp >= lcl) & (pp <= ucl) #vector of 0s and 1s 
RES <-round(cbind(x, sp, m.err, lcl, ucl, prob, cover), 4)
sum(dbinom(x[cover], n,pp)) #total coverage prob at pp
