#Generating a geometric random variable with parameter p

georv <- function(n,p){
  q <- 1-p
  x <- numeric(n)
  
  for(i in 1:n){
  u <- runif(1)
  x[i] <- floor(log(u) / log(q)) + 1
  }
  x
}
georv(10, 0.4)

mean(georv(10000,0.4))

# The R way
mean(replicate(10000, floor(log(runif(1)) / log(1 - 0.4))) + 1)

#Generating a Poisson Random Variable

poisrv <- function(n,l){
  X <- numeric(n)
  for(j in 1:n){
    i <- 0
    p <- exp(-l)
    F <- p
    u <- runif(1)
    while(TRUE){
      if(u < F){
        X[j] <- i
      break }  else{
        p <- (l * p) / (i+1)
        F <- F + p
        i <- i + 1
      }
    }
  }
    X
}
poisrv(20,3)
mean(poisrv(10000,3))


# 2nd algorithm

