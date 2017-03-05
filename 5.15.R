# This is Gamma(2, 1) distribution.
# The expectation is 2, the variance is 2.

N = 1000000

# Directly.
s0 <- rgamma(N, 2, 1)

# The acceptance-rejection method using Exp(1/2).
# f(x) / g(x) = 2 * x * e^(-x / 2) has maximum at x = 2 equal to 4 / e ~ 1.47.
# x is accepted with probability f(x) / g(x) / 4 * e = x * e^(1 - x / 2) / 2.
s1 <- c()
l <- 0
while (l < N) {
  s <- rexp(N - l, 1 / 2)
  s1 <- c(s1, s[which(s / 2 * exp(1 - s / 2) > runif(N - l))])
  l = length(s1)
}

# The acceptance-rejection method using Exp(l) giving the minimum expected
# number of iterations.
# f(x) / g(x) = x / l * e^(-(1 - l) * x) has maximum at x = 1 / (1 - l) equal to
# 1 / (1 - l) / l / e. Minimizing the last expression we obtain l = 1 / 2. Thus,
# l = 1 / 2 is indeed optimal.

# Results
c(mean(s0), var(s0), quantile(s0, c(0.25, 0.5, 0.75)))
c(mean(s1), var(s1), quantile(s1, c(0.25, 0.5, 0.75)))
