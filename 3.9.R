# The sample size
n <- 10000

# The function
f <- function (x, y) {
  if (y < x) exp(-x - y) else 0
}

# The function used in Monte Carlo
h <- function (x, y) {
  f(1 / x - 1, 1 / y - 1) / x^2 / y^2
}

# The boundaries are from 0 to oo for both x and y
# The true value is 0.5

# Monte Carlo
result <- mean(mapply(h, runif(n), runif(n)))
result
