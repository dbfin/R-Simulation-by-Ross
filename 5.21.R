# We already tried to find the best exponential constant in some other exercises.
# f(x) = C * x^(a - 1) * e^(-x), where C = 1 / Gamma(a) / P(Gamma(a, 1) > c)
# g(x) = l * e^(-l * (x - c))
# f(x) / g(x) = C / l * x^(a - 1) * e^((l - 1) * x - l * c)
# If l > 1, then the maximum ratio is +oo as x -> +oo. So, we need l <= 1. Hence,
# the ratio is decreasing in x, so its maximum is at x = c, and is equal to C / l
# * c^(a - 1) * e^(-c). Minimizing w.r.t. l gives l = 1, and the maximum ratio of
# C * c^(a - 1) * e^(-c). For l = a the maximum ratio becomes C / a * c^(a - 1) *
# e^(-c) > C * c^(a - 1) * e^(-c) as a < 1.

N = 1000000

# In what follows, we take a as a random number in (0.1, 0.9) and c = 0.5. Then
# we generate gamma(a, 1) > c directly, and also using l = 1 and l = a. We also
# keep track of the number of random variables required.

# This function generates gamma(a, 1) > c directly or by using
# the acceptance-rejection method. It returns some statistics,
# including the number of uniform random variables required to
# generate the sample. If l = 0, the direct method is used. If
# l > 0, c + Exp(l) are accepted with the probability (x / c)^
# (a - 1)*e^((l - 1) * (x - c)).
ar <- function (N, a, c, l) {
  result <- c()
  sl <- 0
  count <- 0
  if (l > 0) {
    while (sl < N) {
      count <- count + N - sl
      s <- c + rexp(N - sl, l)
      result <- c(result, s[which((a - 1) * log(s / c) + (l - 1) * (s - c) >
                          log(runif(N - sl)))])
      sl <- length(result)
    }
  }
  else {
    while (sl < N) {
      count <- count + N - sl
      s <- rgamma(N - sl, a, 1)
      result <- c(result, s[which(s > c)])
      sl <- length(result)
    }
  }
  c(l, count / N, mean(result), var(result), quantile(result, c(0.25, 0.5, 0.75)))
}

a <- runif(1) * 0.8 + 0.1

all <- rbind(
  ar(N, a, 0.5, 0),
  ar(N, a, 0.5, a),
  ar(N, a, 0.5, (a + 1) / 2),
  ar(N, a, 0.5, 1)
)
colnames(all) <- c("l", "U per X", "E", "V", "25%", "50%", "75%")
a
all
