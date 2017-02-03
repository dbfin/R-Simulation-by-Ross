# First, we can obtain the distribution of X1 as follows:
# P{X1 = k} = C(n1, k) * C(n-n1, m-k) / C(n, m).
# Then, we can generate X1. Once done, we have m-X1 balls
# to draw from the remaining n-n1 balls of the r-1 colors
# that are left. We check the distribution directly.

N <- 5000
ni <- c( 1, 2, 3, 4, 5, 4, 3, 2, 1 )
balls <- unlist(mapply(rep, 1:r, ni))
n <- sum(ni)
r <- length(ni)
m <- 10

# Directly
s1 <- Reduce(c, replicate(N, sample(balls, m, replace = FALSE)))

# One-by-one as described above
X1 <- function (n, n1, m) {
  U <- runif(1) * choose(n, m)
  k <- 0
  Fk <- choose(n - n1, m)
  while (U >= Fk) {
    k <- k + 1
    Fk <- Fk + choose(n1, k) * choose(n - n1, m - k)
  }
  k
}

XSample <- function (ni, m) {
  n <- sum(ni)
  s <- c()
  i <- 1
  while (m > 0) {
    mi <- X1(n, ni[i], m)
    s <- c( s, rep(i, mi) )
    n <- n - ni[i]
    m <- m - mi
    i <- i + 1
  }
  s
}

s2 <- Reduce(c, replicate(N, XSample(ni, m)))
t <- rbind(prop.table(table(s1)), prop.table(table(s2)))
rownames(t) <- c("Directly", "Algorithm")
t
