samplesize <- 10000

# The thinning works as follows
times <- function () {
  t = 0
  times = c()
  while (TRUE) {
    t <- t - log(runif(1)) / 7 # 7 is the maximum rate
    if (t > 10) {
      break
    }
    if (runif(1) <= (3 + 4 / (1 + t)) / 7) {
      times <- c(times, t)
    }
  }
  times
}

sample <- data.matrix(replicate(samplesize, times()))
c(mean(mapply(length, sample)), mean(mapply(mean, sample)),
  mean(mapply(min, sample)), mean(mapply(quantile, sample, 0.25)),
  mean(mapply(median, sample)), mean(mapply(quantile, sample, 0.75)),
  mean(mapply(max, sample)))

# An improved version (there is actually increase in the speed at least 2 times)
times2 <- function () {
  # First, we generate the Poisson process with the parameter l = 3
  times = runif(rpois(1, 3 * 10), 0, 10)
  # Then, we do as before but for the rate decreased by 3
  t = 0
  while (TRUE) {
    t <- t - log(runif(1)) / 4 # 4 is the maximum rate now
    if (t > 10) {
      break
    }
    if (runif(1) <= 1 / (1 + t)) {
      times <- c(times, t)
    }
  }
  times
}

sample <- data.matrix(replicate(samplesize, times2()))
c(mean(mapply(length, sample)), mean(mapply(mean, sample)),
  mean(mapply(min, sample)), mean(mapply(quantile, sample, 0.25)),
  mean(mapply(median, sample)), mean(mapply(quantile, sample, 0.75)),
  mean(mapply(max, sample)))
