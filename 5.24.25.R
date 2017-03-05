# We already tried to find the best exponential constant in some other exercises.
# f(x) = C * e^(-x^2 / 2), where C = sqrt(2 / pi)
# g(x) = l * e^(-l * x)
# f(x) / g(x) = C / l * e^(l * x - x^2 / 2)
# c(l) = sup f(x) / g(x) = f(l) / g(l) = C / l * e^(l^2 / 2)
# inf c(l) = c(1) = C * e^(1 / 2) = 1.315...
# Exp(1) is accepted with probability e^(-(x - 1)^2 / 2)

N = 1000000

# Directly.
s0 <- rnorm(N, 0, 1)

# The acceptance-rejection method (Example 5f).
s1 <- c()
l <- 0
count <- 0
while (l < N) {
  count <- count + N - l
  s <- rexp(N - l, 1)
  s1 <- c(s1, s[which(exp(-(s - 1)^2 / 2) > runif(N - l))])
  l <- length(s1)
}
s1 <- s1 * (rbinom(N, 1, 0.5) * 2 - 1)

c(mean(s0), var(s0), quantile(s0, c(0.005, 0.025, 0.25, 0.5, 0.75, 0.975, 0.995)))
c(mean(s1), var(s1), quantile(s1, c(0.005, 0.025, 0.25, 0.5, 0.75, 0.975, 0.995)))
count / N
