n = 10
N = 1000000

# For testing examples we will use i.i.d. random variables, i.e. Fi = G for each i.
# Then, F^{-1} can be easily expressed via G, allowing the inverse method to check.
# In fact, we will assume all Xi ~ U(0,1).

# (a) X = max(X1, ..., Xn)

# The inverse method to check.
sa0 <- runif(N)^(1 / n)
# The maximum method.
sa1 <- apply(matrix(runif(N * n), ncol = N), 2, max)

# (b) X = min(X1, ..., Xn)

# The inverse method to check.
sb0 <- 1 - (1 - runif(N))^(1 / n)
# The minimum method.
sb1 <- apply(matrix(runif(N * n), ncol = N), 2, min)

c(mean(sa0), var(sa0), quantile(sa0, c(0.25, 0.5, 0.75)))
c(mean(sa1), var(sa1), quantile(sa1, c(0.25, 0.5, 0.75)))
c(mean(sb0), var(sb0), quantile(sb0, c(0.25, 0.5, 0.75)))
c(mean(sb1), var(sb1), quantile(sb1, c(0.25, 0.5, 0.75)))
