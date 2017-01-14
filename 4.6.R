# Things such as generating a Bernoulli sample, are done behind the scenes
# in R, you just use, for example, the sample function. However, this task
# can be considered as the one where you want to calculate the mean number
# of geometric random variables required to generate a Bernoulli sample.

# p
p <- 0.8

# Bernoulli sample sizes
n <- c(1, 2, 3, 4, 5, 10, 15, 20, 25, 50, 100, 1000)

# The number of trials for each size
N <- 10000

# A function that generates a Bernoulli sample, and returns the number of
# geometric random variables required.
B <- function (p, n) {
  if (p > 0.5) p <- 1 - p
  count <- 0
  samplesize <- 0
  while (samplesize < n) {
    samplesize <- samplesize + rgeom(1, p) + 1
    count <- count + 1
  }
  count
}

# A function to perform B many times and calculate the average
B_average <- function (N, p, n) {
  mean(replicate(N, B(p, n)))
}

# The average number of required geometric random variables should be close to
# n * min{ p, 1 - p } for large values of n

# Counting the average number of geometric variables required for different n
rbind(n, mapply(B_average, N, p, n))
