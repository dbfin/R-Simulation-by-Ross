# The expectation and variance are n / (n + 1) and n / (n + 2) / (n + 1)^2.
# For n = 3, they are 0.75 and 0.0375.

n = 3

N = 1000000

# Method 0: the inverse method
s0 <- (runif(N))^(1 / n)

# Method 1: the acceptance-rejection method
# The density n * x^(n - 1) has maximum at x = 1 equal to n.
s1 <- c()
while (length(s1) < N) {
  s <- runif(N - length(s1))
  s1 <- c(s1, s[which(s^(n - 1) > runif(N - length(s1)))])
}

# Method 2: the method of Exercise 12
s2 <- apply(matrix(runif(N * n), nrow = n), 2, max)

c(mean(s0), var(s0), quantile(s0, c(0.25, 0.5, 0.75)))
c(mean(s1), var(s1), quantile(s1, c(0.25, 0.5, 0.75)))
c(mean(s2), var(s2), quantile(s2, c(0.25, 0.5, 0.75)))
