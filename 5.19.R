N = 1000000

# The inverse method.
# F^{-1}(u) = (sqrt(1 + 8 * u) - 1) / 2
s0 <- (sqrt(1 + 8 * runif(N)) - 1) / 2

# The acceptance-rejection.
# f(x) = x + 1 / 2
# g(x) = 1
# c = 3 / 2
s1 <- c()
l <- 0
while (l < N) {
  s <- runif(N - l)
  s1 <- c(s1, s[which(2 * s / 3 + 1 / 3 > runif(N - l))])
  l <- length(s1)
}

# The composition method.
# We generate U and take its square root with probability 1 / 2.
# Alternatively, we take U * 2 and if the result is greater than
# 1, then we take sqrt(U * 2 - 1)
s2 <- runif(N) * 2
i <- which(s2 > 1)
s2[i] <- sqrt(s2[i] - 1)

c(mean(s0), var(s0), quantile(s0, c(0.25, 0.5, 0.75)))
c(mean(s1), var(s1), quantile(s1, c(0.25, 0.5, 0.75)))
c(mean(s2), var(s2), quantile(s2, c(0.25, 0.5, 0.75)))
