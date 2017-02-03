# If each ln = l, we have pn = (1-l)^(n-1) * l, which is the geometric distribution
# on 1, 2, ... So, the geometric distribution corresponds to the case of a constant
# hazard rate.

n = 100000

l = 0.9

U <- runif(n)
f <- c()
j <- 1
Fj <- 1
while (sum(f) < n) {
  f[j] <- length(which(U >= Fj * (1 - l) & U < Fj))
  j <- j + 1
  Fj <- Fj * (1 - l)
}

rbind(1:length(f), f / n)

# Now we take different hazard rates. For example, ln = 0.4, 0.5, 2/3, 2/3, ...
# then pn are 0.4, 0.3, 0.2, 0.1 * 2/3, 0.1 * 1/3 * 2/3, 0.1 * 1/3^2 * 2/3, ...

# Note that ln <= 2/3 for all n, and we can use the algorithm of 19 with l=2/3.
# In the algorithm, Y = k iff (1-l)^(k-1) >= U > (1-l)^k, so the probability of
# Y = k is l*(1-l)^(k-1), so Y has the geometric distribution with parameter k.

# What is going on here is that we increase the hazard rate at each step, and this
# allows as to generate a series of Bernoulli observations as the geometric random
# variable, and then if the hazard event does not happen at a certain stage at the
# increased hazard rate, we assume it does not happen at the lower rate, otherwise
# if the hazard event happens at the higher rate l, we assume that it happened for
# real with the probability ln / l. Therefore, the probability that X = k is equal
# to the probability that X >= k times l * ln / l = ln, exactly what we need.

X <- function () {
  X <- 0
  l <- 2/3
  while (TRUE) {
    X <- X + floor(log(runif(1)) / log(1 - l)) + 1
    if (X <= 2) {
      lX <- (c( 0.4, 0.5 ))[X]
    }
    else {
      lX <- l
    }
    if (runif(1) < lX / l) {
      break
    }
  }
  X
}

s <- replicate(n, X())
prop.table(table(s))
