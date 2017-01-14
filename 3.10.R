# The sample size
n <- 1000000

# Cov(U, expU) = E(U expU) - EU EexpU
#              = int(0,1) x exp(x) dx - (1/2) int(0,1) exp(x) dx
#              = (3 - e) / 2
#              = 0.140859

# Monte Carlo
x <- runif(n)
result <- mean(x * exp(x)) - mean(x) * mean(exp(x))
result
