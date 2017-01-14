# The sample size
n <- 1000000

# The function
f <- function (x) {
  exp(exp(x))
}

# The boundaries
min <- 0
max <- 1
# [0,1]: the true value is 6.31656
# [-1, 2]: the true value is 263.906

# Monte Carlo
result <- mean((max - min) * f(runif(n, min = min, max = max)))
result
