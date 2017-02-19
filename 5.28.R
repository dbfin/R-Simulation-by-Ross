# Let N be the random value equal to the number of stages completed
# Then, if Ti is the time spent on stage i, the total time spent in
# the system is T = sum(i = 1, ..., k) Ti * I(N >= i)
# We have that P(N >= i) = a1 * ... * a(i-1), hence,
# ET = sum(i = 1, ..., k) a1 * ... * a(i-1) / li

# Suppose k = 10, li = i, ai = (10 - i) / 10
# Then, ET = sum(i = 1, ..., 10) 9! / (10 - i)! / 10 ^ (i - 1) / i = 1.913...

samplesize <- 100000

k <- 10
li <- 1:10
ai <- 1 - 1:9 / 10

observation <- function (k, li, ai) {
  N = 1
  while (N < k && runif(1) <= ai[N]) {
    N <- N + 1
  }
  t = 0
  i = 1
  while (i <= N) {
    t <- t + rexp(1, li[i])
    i <- i + 1
  }
  t
}

sample <- replicate(samplesize, observation(k, li, ai))
c(mean(sample))
