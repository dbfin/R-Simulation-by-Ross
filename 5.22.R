# This is just Beta(3, 3).
# The expectation is 1 / 2.
# f(x) = 30 * x^2 * (1-x)^2
# g(x) = 1
# c = sup f(x) / g(x) = 30 * 1 / 4 * 1 / 4 = 30 / 16 = 15 / 8

N = 10000000

# Directly.
s0 <- rbeta(N, 3, 3)

# The acceptance-rejection method.
s1 <- c()
l <- 0
count <- 0
while (l < N) {
  count <- count + N - l
  s <- runif(N - l)
  s1 <- c(s1, s[which(16 * (s * (1 - s))^2 > runif(N - l))])
  l <- length(s1)
}

c(mean(s0), var(s0), quantile(s0, c(0.25, 0.5, 0.75)))
c(mean(s1), var(s1), quantile(s1, c(0.25, 0.5, 0.75)))
count / N
