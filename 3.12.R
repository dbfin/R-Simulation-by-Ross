# The sample size
n <- c(100, 1000, 10000, 100000)

# The function that returns a random N value
N <- function() {
  N = 0
  sum = 0
  while (sum <= 1) {
    N <- N + 1
    sum <- sum + runif(1)
  }
  N
}

# P(N > k) = P(U1 + ... + Uk <= 1) = 1/k!
# E(N) = sum(k=0,1,2,...) P(N > k) = e

# Monte Carlo
N_sample <- function(n) {
  replicate(n, get_N())
}
mapply(mean, mapply(N_sample, n))
