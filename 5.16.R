# The density is given by f(x) = e^(-x) + 2 * e^(-2x) - 3 * e^(-3x).
# The expectation 1 + 1 / 2 - 1 / 3 = 7 / 6 = 1.16666...
# The variance is 2 + 2 / 4 - 2 / 9 - (7 / 6)^2 = 11 / 12 = 0.91666...

N = 1000000

# The inverse method.
# Let t = e^(-x), then 1 - t - t^2 + t^3 = 1 - u implies t^3 - t^2 - t + u = 0.
# The derivative of this polynomial is 3 * t^2 - 2 * t - 1 with roots t = -1/3
# and t = 1, and the polynomial decreases between 0 and 1 from u to u - 1. So,
# there is a unique root between 0 and 1 for each u between 0 and 1. Using the
# trigonometric formula for cubic roots, the expressions are as follows.
# x = t + 1 / 3
# p = -4 / 3
# q = u - 11 / 27
# t^3 + p * t + q = 0
# the 2ndroot is
# t = 4 / 3 * cos(arccos(11 / 16 - 27 / 16 * u) / 3 - 2 * pi / 3)
s0 <- -log(4/3 * cos(acos(11 / 16 - 27 / 16 * runif(N)) / 3 - 2 * pi / 3) + 1/3)

# The acceptance-rejection method
# Let g(x) = e^(-x) (for simplicity), then f(x) / g(x) = 1 + 2 * e^(-x) - 3 *
# e^(-2x), with maximum at x = log(3) equal to 4 / 3. Hence, we generate x~g,
# and accept it with probability 3 / 4 + 3 / 2 * e^(-x) - 9 / 4 * e^(-2x).
s1 <- c()
l <- 0
while (l < N) {
  s <- exp(-rexp(N - l, 1))
  s1 <- c(s1, s[which(3 / 4 + 3 / 2 * s - 9 / 4 * s^2 > runif(N - l))])
  l <- length(s1)
}
s1 <- -log(s1)

c(mean(s0), var(s0), quantile(s0, c(0.25, 0.5, 0.75)))
c(mean(s1), var(s1), quantile(s1, c(0.25, 0.5, 0.75)))
