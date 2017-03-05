# This is kind of obvious because F_R(x) = P(R <= x) is proportional to the area
# of the circle of radius x, and F_R(1) = 1, hence, F_R(x) = x^2. Then, F_R(R) =
# R^2 ~ U[0, 1]. In other words, 0 <= R^2 <= 1, P(R^2 < x) = P(R < sqrt(x)) = x.

N = 1000000

# Directly.
# We generate pairs (X, Y) and accept them if they are in the circle.
s0 <- c()
l <- 0
count0 <- 0
while (l < N) {
  count0 <- count0 + 2 * (N - l)
  X <- runif(N - l) * 2 - 1
  Y <- runif(N - l) * 2 - 1
  R2 <- X^2 + Y^2
  i <- which(R2 <= 1)
  s0 <- cbind(s0, rbind(X[i], Y[i]))
  l <- l + length(i)
}
s0 <- rbind(s0, sqrt(s0[1,]^2 + s0[2,]^2))

# Circular fanning.
R <- sqrt(runif(N))
a <- runif(N) * 2 * pi
X <- R * cos(a)
Y <- R * sin(a)
s1 <- rbind(X, Y, R)
count1 <- 2 * N

all <- rbind(
  c(mean(s0[1,]), var(s0[1,]), quantile(s0[1,], c(0.25, 0.5, 0.75))),
  c(mean(s0[2,]), var(s0[2,]), quantile(s0[2,], c(0.25, 0.5, 0.75))),
  c(mean(s0[3,]), var(s0[3,]), quantile(s0[3,], c(0.25, 0.5, 0.75))),
  c(mean(s1[1,]), var(s1[1,]), quantile(s1[1,], c(0.25, 0.5, 0.75))),
  c(mean(s1[2,]), var(s1[2,]), quantile(s1[2,], c(0.25, 0.5, 0.75))),
  c(mean(s1[3,]), var(s1[3,]), quantile(s1[3,], c(0.25, 0.5, 0.75)))
)
colnames(all)[1:2] <- c("E", "V")
rownames(all) <- c("Direct X", "Direct Y", "Direct R",
                   "Fanning X", "Fanning Y", "Fanning R")
all
count0
count1
