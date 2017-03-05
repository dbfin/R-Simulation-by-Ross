# We give two methods, and two algorithms for one of them.

N = 1000000

# The inverse method.
# The distribution function is F(x) = 1 - e^(-x^2).
# So, F^{-1}(u) = sqrt(-log(1 - u))
s0 <- sqrt(-log(1 - runif(N)))

# The acceptance-rejection method 1.
# If g(x) = l * e^(-l * x), then f(x) / g(x) = 2 / l * x * e^(l * x - x^2).
# Solving for argmin[l] max[x] (f(x) / g(x)) gives l = 1, and c = 2. Hence,
# we generate Exp(1), and accept it if U < x * e^(x - x^2).
s1 <- c()
l <- 0
while (l < N) {
  s <- rexp(N - l, 1)
  s1 <- c(s1, s[which(s * exp(s - s^2) > runif(N - l))])
  l <- length(s1)
}

# The acceptance-rejection method 2.
# If g(x) = l^2 * x * e^(-l * x), then f(x) / g(x) = 2 / l^2 * e^(l * x - x^2).
# Solving for argmin[l] max[x] (f(x) / g(x)) we get l = 2, c = e / 2 = 1.359...
# Hence, we generate Gamma(2, 2), and accept it if U < e^(-(x - 1)^2).
s2 <- c()
l <- 0
while (l < N) {
  s <- rgamma(N - l, 2, 2)
  s2 <- c(s2, s[which(exp(-(s - 1)^2) > runif(N - l))])
  l <- length(s2)
}

c(mean(s0), var(s0), quantile(s0, c(0.25, 0.5, 0.75)))
c(mean(s1), var(s1), quantile(s1, c(0.25, 0.5, 0.75)))
c(mean(s2), var(s2), quantile(s2, c(0.25, 0.5, 0.75)))
