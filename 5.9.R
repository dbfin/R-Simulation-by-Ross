# We use two methods to verify the results.


N <- 10000000


# The inverse method.

# F(x) = 1 / (1 - log(x))
# The expected value is 0.40365...
# F^(-1)(u) = e^(1 - 1 / u)

s1 <- exp(1 - 1 / runif(N))


# The method suggested in the hint.

# I am not sure how this is related to Exercise 7, but
# the reason the distribution function is written this
# way is that we may assume that Y ~ exp(1), and then,
# conditional on Y=y, X has distribution x^y, which is
# simple enough to use the inverse method.

Y <- rexp(N, 1)
s2 <- runif(N) ^ (1 / Y)


c(mean(s1), quantile(s1, c(0.25, 0.5, 0.75)))
c(mean(s2), quantile(s2, c(0.25, 0.5, 0.75)))
