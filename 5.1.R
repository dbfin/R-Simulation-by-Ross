# The theoretical value of the expectation is EX = 1 / (e - 1) = 0.5819767...

# We use three methods
# 1) X = 1 - (Y | Y <= 1) where Y ~ Exp(1) (the same as the third method)
# 2) The inverse method (the fastest)
# 3) The acceptance-rejection method (the same as the first method)

# It is interesting to compare the first and the third methods. Assume
# we need to generate X ~ f(x) conditional on X being in [a, b]. Then,
# we can either 1) generate X until it is in [a, b], or 2) generate it
# using the acceptance-rejection method, e.g., we generate U ~ U[a, b]
# and we accept it with the probability f(U) / sup {x in [a, b]} f(x).
# Note that the larger the interval [a, b], the higher the probability
# of X being in it, but the lower can the probability of acceptance in
# the acceptance-rejection method. And vice versa. Also, if we compare
# not just the expected numbers of generated random variables, but the
# methods used to generate them, then, in 1) we generate X ~ f(x), but
# in 2) we generate U ~ U[a, b] and calculate f(U). Depending on f(x),
# either method may have further advantage over the other. Overall, we
# conclude that either method can be preferrable.

# Compare to Exercise 6. Here the first and the third methods require
# the same number of random variables, but in Exercise 6 the interval
# is small.


N = 10000000 # to see the difference in speed
e = exp(1)


# 1) Using the conditional exponential distribution

# The conditional exponential distribution can be generated using
# the acceptance-rejection method, but we use it directly for the
# distribution of X itself below. Alternatively, we just reject a
# value of Y if it is above 1. Note that a rejection then happens
# with the probability P(Y > 1) = 1 / e. Thus, on average we need
# e / (e - 1) ~ 1.5819767... Ys for each X.

s0 <- c()
while (length(s0) < N) {
  s <- rexp(N - length(s0), 1)
  s0 <- c(s0, 1 - s[which(s <= 1)])
}


# 2) Using the inverse method

# F(x) = int[0,x] e^y / (e - 1) dy = (e^x - 1) / (e - 1)
# F^-1(u) = log(1 + (e - 1) * u)

s1 <- log(1 + (e - 1) * runif(N))


# 3) Using the acceptance-rejection method

# f(x) <= f(1) = e / (e - 1) ~ 1.5819767... Us for each X
# We generate U, V ~ U[0,1], and accept U if V <= f(U) / f(1) = e^(U - 1)

s2 <- c()
while (length(s2) < N) {
  s <- runif(N - length(s2))
  s2 <- c(s2, s[which(s >= log(runif(N - length(s2))) + 1)])
}

c(mean(s0), quantile(s0, c(0.25, 0.5, 0.75)))
c(mean(s1), quantile(s1, c(0.25, 0.5, 0.75)))
c(mean(s2), quantile(s2, c(0.25, 0.5, 0.75)))
