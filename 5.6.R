# The theoretical value of the expectation is EX = 0.02479...

# We use three methods
# 1) The direct method to generate ( X ~ Exp(1) | X < 0.05 ) (slow)
# 2) The inverse method (the fastest)
# 3) The acceptance-rejection method (as fast as the second method)

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

# Compare to Exercise 1. There the first and the third methods require
# the same number of random variables, but in in this one the interval
# is small.


N = 2500000 # to see the difference in speed (we set this value 4 times
            # smaller than in Exercise 1 due to method 1)
e = exp(1)


# 1) The direct method to generate the conditional exponential distribution

# We just reject X if it is above 0.05. Note that a rejection then happens
# with the probability P(X > 0.05) = e^(-0.05) ~ 0.95. Thus, on average we
# need about 20 Xs for each obervation.

s0 <- c()
while (length(s0) < N) {
  s <- rexp(N - length(s0), 1)
  s0 <- c(s0, s[which(s <= 0.05)])
}


# 2) Using the inverse method

# F(x) = int[0,x] e^(-y) / (1 - e^(-0.05)) dy = (1 - e^(-x)) / (1 - e^(-0.05))
# F^-1(u) = - log(1 - (1 - e^(-0.05) * u)

s1 <- - log(1 - (1 - e^(-0.05)) * runif(N))


# 3) Using the acceptance-rejection method

# f(x) / (1 / 0.05) <= f(0) / 20 = 1.025... Us for each X
# We generate U ~ U[0, 0.05], V ~ U[0,1], and accept U if
# V <= f(U) / f(0) = e^(-U)

s2 <- c()
while (length(s2) < N) {
  s <- runif(N - length(s2), 0, 0.05)
  s2 <- c(s2, s[which(s <= - log(runif(N - length(s2))))])
}

c(mean(s0), quantile(s0, c(0.25, 0.5, 0.75)))
c(mean(s1), quantile(s1, c(0.25, 0.5, 0.75)))
c(mean(s2), quantile(s2, c(0.25, 0.5, 0.75)))
