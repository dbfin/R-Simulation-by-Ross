n = 10000
r = 10
p = 1 / 3


# The R way

# Note that the standard function returns the number of failures
# before r successes, so we need to add r
result <- rnbinom(n, r, p) + r
result_0 <- c( mean(result), var(result), min(result), max(result) )


# (a) The method is to use the sum of r Geometric random variables

Geometric <- function (n, p) {
  # Pages 51-52
  floor(log(runif(n)) / log(1 - p)) + 1
}

NegativeBinomial_a <- function (n, r, p) {
  colSums(matrix(Geometric(n * r, p), nrow = r, ncol = n))
}

result <- NegativeBinomial_a(n, r, p)
result_a <- c( mean(result), var(result), min(result), max(result) )


# (c) The method is analogous to the one on page 54

NegativeBinomial_c_from_U <- function (U, r, p) {
  j <- r
  pj <- p**r
  Fj <- pj
  while (U >= Fj) {
    pj <- j * (1 - p) * pj / (j + 1 - r)
    Fj <- Fj + pj
    j <- j + 1
  }
  j
}

NegativeBinomial_c <- function (n, r, p) {
  mapply(NegativeBinomial_c_from_U, runif(n), r, p)
}

result <- NegativeBinomial_c(n, r, p)
result_c <- c( mean(result), var(result), min(result), max(result) )


# (d) The straightforward method

NegativeBinomial_d_single <- function (r, p) {
  count <- 0
  while (r > 0) {
    count <- count + 1
    if (runif(1) < p) {
      r <- r - 1
    }
  }
  count
}

NegativeBinomial_d <- function (n, r, p) {
  replicate(n, NegativeBinomial_d_single(r, p))
}

result <- NegativeBinomial_d(n, r, p)
result_d <- c( mean(result), var(result), min(result), max(result) )

rbind(result_0, result_a, result_c, result_d)
