# Instead of plotting it, we would rather do this many times and obtain statistics
# We will also do this in two different methods

samplesize = 1000
R = 5
lambda = 1

distancequantile <- function (XYs, q) {
  R <- sqrt(XYs[1,] ^ 2 + XYs[2,] ^ 2)
  if (q == 0.0) {
    min(R)
  }
  else if (q == 1.0) {
    max(R)
  }
  else {
    quantile(R, q)
  }
}

# Circular fanning
observation <- function (R, lambda) {
  sums = c()
  sum = 0
  while (TRUE) {
    X <- rexp(1, lambda)
    sum <- sum + X
    if (sum < pi * R^2) {
      sums <- c(sums, sum)
    }
    else {
      break
    }
  }
  
  Rs <- sqrt(sums / pi)
  angles <- runif(length(Rs), 0, 2 * pi)
  Xs <- Rs * cos(angles)
  Ys <- Rs * sin(angles)
  
  rbind(Xs, Ys)
}

sample <- replicate(samplesize, observation(R, lambda))
c(mean(mapply(ncol, sample)),
  mean(mapply(distancequantile, sample, 0.0)),
  mean(mapply(distancequantile, sample, 0.25)),
  mean(mapply(distancequantile, sample, 0.5)),
  mean(mapply(distancequantile, sample, 0.75)),
  mean(mapply(distancequantile, sample, 1.0)))

# Horizontal fanning
observation2 <- function (R, lambda) {
  sums = c()
  sum = 0
  while (TRUE) {
    X <- rexp(1, lambda)
    sum <- sum + X
    if (sum < 4 * R^2) {
      sums <- c(sums, sum)
    }
    else {
      break
    }
  }
  
  Xs <- sums / 2 / R - R
  Ys <- runif(length(Xs), -R, R)

  result <- rbind(Xs, Ys, Xs ^ 2 + Ys ^ 2)
  data.matrix(result[1:2, result[3,] < R ^ 2])
}

sample <- replicate(samplesize, observation2(R, lambda))
c(mean(mapply(ncol, sample)),
  mean(mapply(distancequantile, sample, 0.0)),
  mean(mapply(distancequantile, sample, 0.25)),
  mean(mapply(distancequantile, sample, 0.5)),
  mean(mapply(distancequantile, sample, 0.75)),
  mean(mapply(distancequantile, sample, 1.0)))
