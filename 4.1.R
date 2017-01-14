# The sample size
n <- c(100, 1000, 10000, 100000)

# The function that returns a sample of size n
X_sample <- function(n) {
  round(runif(n) * 1.5) + 1
}

# Generating samples and counting the proportions of 1's
#   We use 2 - mean to determine the proportion of 1's
result <- 2 - mapply(mean, (mapply(X_sample, n)))
result
