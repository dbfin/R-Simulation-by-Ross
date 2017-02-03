l <- 2
k <- 10
n <- 100000

# First, we can employ the method based on the observation that
# p0 = 1 / (1 + sum[j=1,...,k]( l^j / j! ))
# p(i+1) = pi * l / (i+1)

s <- runif(n)
f1 <- numeric(k + 1)

j <- 0
pj <- 1 / sum(l**(0:k) / factorial(0:k))
Fj <- pj
Fjprevious <- 0
while (sum(f1) < n) {
  f1[j + 1] <- length(which(s >= Fjprevious & s < Fj))
  pj <- pj * l / (j + 1)
  Fjprevious <- Fj
  Fj <- Fj + pj
  j <- j + 1
}

# Second, we can use the acceptance-rejection method, which becomes just to draw
# from Poisson distribution until the result is <= k

f2 <- numeric(k + 1)
while (sum(f2) < n) {
  s <- rpois(n - sum(f2), l)
  for (j in 0:k) {
    f2[j + 1] <- f2[j + 1] + length(which(s == j))
  }
}

rbind(l**(0:k) / factorial(0:k) / sum(l**(0:k) / factorial(0:k)), f1 / n, f2 / n)
