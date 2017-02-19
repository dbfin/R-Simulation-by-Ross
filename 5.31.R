# Of course, using the single lambda for the whole interval would not be efficient
# In fact, we can split the interval into many pieces, and we'll do this
# for the second subinterval only, namely for each increase of lambda by 1

samplesize = 10000

# The direct approach
times <- function () {
  t = 0
  times = c()
  while (TRUE) {
    t <- t - log(runif(1)) / 26 # 26 is the maximum rate
    if (t > 10) {
      break
    }
    if (t < 5) {
      if (runif(1) <= t / 5 / 26) {
        times <- c(times, t)
      }
    }
    else {
      if (runif(1) <= (5 * t - 24) / 26) {
        times <- c(times, t)
      }
    }
  }
  times
}

sample <- data.matrix(replicate(samplesize, times()))
c(mean(mapply(length, sample)), mean(mapply(mean, sample)),
  mean(mapply(min, sample)), mean(mapply(quantile, sample, 0.25)),
  mean(mapply(median, sample)), mean(mapply(quantile, sample, 0.75)),
  mean(mapply(max, sample)))

# We divide the interval into [0,5), [5,5.2), ..., [9.8,10)
# For the ith interval we generate Poisson(i) + thinning with lambda = 1
# where lambda(t) = (t - t(i-1)) / (t(i) - t(i - 1))
# We treat i = 0 separately
# Note how faster this is

# This function returns a Poisson process on [0, l) with lambda(t) = t / l
thinning <- function (l) {
  times <- c()
  t = 0
  while (TRUE) {
    t <- t - log(runif(1))
    if (t > l) {
      break
    }
    if (runif(1) <= t / l) {
      times <- c(times, t)
    }
  }
  times
}

# This function does the main work
times <- function () {
  ti = c(0:25 / 5 + 5)
  times = thinning(5)
  j = 1
  while (j < length(ti)) {
    times <- c(times, runif(rpois(1, j * (ti[j + 1] - ti[j])),
               ti[j], ti[j + 1]),
               thinning(ti[j + 1] - ti[j]) + ti[j])
    j <- j + 1
  }
  times
}

sample <- data.matrix(replicate(samplesize, times()))
c(mean(mapply(length, sample)), mean(mapply(mean, sample)),
  mean(mapply(min, sample)), mean(mapply(quantile, sample, 0.25)),
  mean(mapply(median, sample)), mean(mapply(quantile, sample, 0.75)),
  mean(mapply(max, sample)))
