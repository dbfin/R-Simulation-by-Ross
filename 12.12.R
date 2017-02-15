# Note: I believe it should be n-i+b-1. Also, of course, 0 <= y <= 1.
# Consider the following model:
# - N is chosen ~ Poisson(lambda),
# - Y is chosen ~ Beta(alpha, beta),
# - X is chosen ~ B(n, y).
# Then, P{X = i, y <= Y <= y + dy, N = n} is as given.

alpha = 2
beta = 3
lambda = 4

sample = 10000


# First, we are going to calculate the expectations theoretically:

# EN = lambda = 4
# EY = alpha / (alpha + beta) = 0.4
# EX = E(E(X|Y,N)) = E(NY) = EN EY = 1.6


# Second, we estimate the averages directly:

N <- rpois(sample, lambda)
Y <- rbeta(sample, alpha, beta)
X <- mapply(rbinom, rep(1), N, Y)

c(mean(N), mean(Y), mean(X))


# Third, we use MCMC

# We consider the vector of (N, Y, X) and use the Gibbs sampler
# P(N = n | X = i, Y ~ y) = P(N = n, X = i, Y ~ y) / P(X = i, Y ~ y) ~
#   C(n, i) y^(i + a - 1) (1-y)^(n - i + b - 1) e^l l^n / n! dy /
#   sum[n >= i] C(n, i) y^(i + a - 1) (1-y)^(n - i + b - 1) e^l l^n / n! dy ~
#   C(n, i) (1-y)^n l^n / n! ~
#   (1-y)^n l^n / (n - i)! ~
#   i + Poisson((1 - y)l)
# P(Y ~ y | X = i, N = n) = P(Y ~ y, X = x, N = n) / P(X = x, N = n) ~
#   C(n, i) y^(i + a - 1) (1-y)^(n - i + b - 1) e^l l^n / n! dy /
#   int[0,1] C(n, i) y^(i + a - 1) (1-y)^(n - i + b - 1) e^l l^n / n! dy ~
#   y^(i + a - 1) (1-y)^(n - i + b - 1) dy ~
#   Beta(i + a, n - i + b)
# (X | Y = y, N = n) ~
#   B(n, y)

MoveToTheNextState <- function (currentState) {
  N <- currentState[1]
  Y <- currentState[2]
  X <- currentState[3]
  U <- runif(1)
  if (U < 1 / 3) { # change N
    N <- X + rpois(1, (1 - Y) * lambda)
  }
  else {
    if (U < 2 / 3) { # change Y
      Y <- rbeta(1, X + alpha, N - X + beta)
    }
    else { # change X
      X <- rbinom(1, N, Y)
    }
  }
  c(N, Y, X)
}

currentState <- c(1, 0.5, 0)
states <- matrix(currentState, ncol = 1)
while (ncol(states) < sample) {
  currentState <- MoveToTheNextState(currentState)
  states <- cbind(states, currentState)
}

rowMeans(states)
