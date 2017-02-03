# This problem is good because it shows that intuitively plausible solutions
# may not work!

n = 10
k = 4
r = 3
N = 100000

# The most efficient way would be to generate a random element from {1, ..., k},
# and then generate a subset of size r-1 from the rest of the elements...
# This does not work as in fact this would increase the probability of sets
# having more than 1 element among {1, ..., k}, see results

RandomSubsetWith1K_wrong <- function(r, n, k) {
  first <- floor(runif(1) * k) + 1
  sort(c( first, sample( (1:n)[-first], r - 1, replace = FALSE )))
}

result <- replicate(N, RandomSubsetWith1K_wrong(r, n, k))
df <- data.frame(t(result), count=1/N)
aggregate(count ~ ., df, sum)

# The probability that i from 1 to k is the smallest element of the subset
# is proportional to (n-i)!/(n-i-r+1)!, and for i=1 it is equal to
# p1 = 1 / [ n / r - C(n - k, r) / C(n - 1, r - 1) ],
# where, for i>=1,
# p(i+1) = (n-i-r+1) / (n-i) * pi
# Accordingly, we can generate the smallest element, and then a subset
# with r-1 elements greater than this element
# This should work (we do not check the condition r + k < n)

RandomSubsetWith1K <- function (r, n, k) {
  j <- 1
  pj <- 1 / ( n / r - choose(n - k, r) / choose(n - 1, r - 1) )
  Fj <- pj
  U <- runif(1)
  while (U >= Fj) {
    pj <- (n - j - r + 1) / (n - j) * pj
    Fj <- Fj + pj
    j <- j + 1
  }
  c( j, sort(sample((j+1):n, r - 1, replace = FALSE)))
}

result <- replicate(N, RandomSubsetWith1K(r, n, k))
df <- data.frame(t(result), count=1/N)
aggregate(count ~ ., df, sum)
