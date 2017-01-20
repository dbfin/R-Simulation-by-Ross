#Example 4b

#discrete uniform random number generation from 1:n
# Note that you choose n random numbers from 1 to n, so I am not sure where you use this
# The R way: sample(1:n, n, replace = TRUE)
discreteuniform <- function(n){
  X <- numeric(n)
  for(i in 1:n){
    U <- runif(1)
    # Why would you check all j? The result is just
    X[i] <- floor(n * U) + 1
  }
  X
}

Y <- discreteuniform(3000)
mean(Y==10)


#choosing k random numbers from discreteuniform(n)
# The R way: sample(1:n, k, replace = TRUE)
kchoosen <- function(n,k){
  X <- numeric(k)
  for(i in 1:k){
    U <- runif(1)
    # Again, j is not used
    X[i] <- floor(n * U) + 1
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

# The R way: sample(1:n, n, replace = FALSE)
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

# this is ok, but you may want to think how to test the distribution of permutations
perm(4)

#generating a random subset of size r of the integers 1, ... , n.
# The R way: sample(1:n, k, replace = FALSE)
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

# And test it here as well
r_perm_n(10,3)
