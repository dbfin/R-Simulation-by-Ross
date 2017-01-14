#Example 4a 

#First method
random_X <- function(n){
  X <- numeric(n)
  for(i in 1:n){
    U <- runif(1, 0, 1)
if(U < 0.2){
  X[i] <- 1
}else if(U < 0.35){
  X[i] <- 2
}else if(U < 0.6){
  X[i] <- 3
}else X[i] <- 4

  }
  X
}
Z <- random_X(100000)
mean(Z==3)
#Second method
random_X2 <- function(n){
  X <- numeric(n)
  for(i in 1:n){
    U <- runif(1, 0, 1)
    if(U < 0.4){
      X[i] <- 4
    }else if(U < 0.65){
      X[i] <- 3
    }else if(U < 0.85){
      X[i] <- 1
    }else X[i] <- 2
    
  }
  X
}
Y <- random_X2(100000)
mean(Y==3)
