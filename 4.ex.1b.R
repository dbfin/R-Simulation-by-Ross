#Example 4b

#discrete uniform random number generation from 1:n

discreteuniform <- function(n){
  X <- numeric(n)
  for(i in 1:n){
    U <- runif(1)
    for(j in 1:n){
      if(j-1 <= n*U & n*U <j){
        X[i] <- floor(n * U) + 1
        break
      }
    }
  }
  X
}

Y <- discreteuniform(3000)
mean(Y==10)


#choosing k random numbers from discreteuniform(n)

kchoosen <- function(n,k){
  X <- numeric(k)
  for(i in 1:k){
    U <- runif(1)
    for(j in 1:n){
      if(j-1 <= n*U & n*U <j){
        X[i] <- floor(n * U) + 1
        break
      }
    }
  }
  X
}
kchoosen(1000,4)


#Generating a Random Permutation

n <- 4
x <- 1:n;
y <-x

x[4] <- y[3]; x[3] <-y[4]; x

x[3] = y[2]; x[2] = y[3]; x

x[2] = y[2]; x

perm <- function(n){
  x <- 1:n
  for(k in n:2){
    y <- x
    u <- runif(1)
    I <- floor(k * u) + 1
    x[k] <- y[I]; x[I] <- y[k]
  }
  x
}

perm(4)

#generating a random subset of size r of the integers 1, ... , n.

r_perm_n <- function(n,r){
  x <- 1:n
  for(k in n:(n-r+1)){
    y <- x
    u <- runif(1)
    I <- floor(k * u) + 1
    x[k] <- y[I]; x[I] <- y[k]
  }
  x[n:(n-r+1)]
}

r_perm_n(10,3)




