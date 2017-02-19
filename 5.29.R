# Of course, using the submartingale theory, we know that
# the expected number of fans is EN * EX_i = 5 * 30 = 150

samplesize = 10000

# The direct approach
fans <- function() {
  t = 0
  n = 0
  while (TRUE) {
    t <- t + rexp(1, 5)
    if (t > 1) {
      break;
    }
    n <- n + floor(runif(1) * 21) + 20
  }
  n
}

sample <- replicate(samplesize, fans())
c(mean(sample), min(sample), quantile(sample, 0.25),
  median(sample), quantile(sample, 0.75), max(sample))

# Since we only need to know the number of busses, the following will work as well
sample <- replicate(samplesize, sum(floor(runif(rpois(1, 5)) * 21 + 20)))
c(mean(sample), min(sample), quantile(sample, 0.25),
  median(sample), quantile(sample, 0.75), max(sample))
