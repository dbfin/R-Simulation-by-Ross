# This is called the Coupon Collecting Problem. See, for example, Ross,
# Introduction to Probability Models, Example 5.17. There are different
# formulas for the expected number of trials to collect all objects. If
# the probabilities of outcomes are the same, the formula can be easily
# obtained. In the general case of different probabilities, the formula
# given in Ross states that the average number of trials needed is
#   int(0,oo) [ 1 - prod(j=2,...,12)[1 - exp(-pj t)] ] dt
#   = ( Wolfram Alpha ) = 769767316159 / 12574325400 ~ 61.217

# The sample size
n <- 10000

# A function that returns the number of trials needed
f <- function () {
  trials <- 0
  collected <- numeric(12)
  collectedn <- 0
  while (collectedn < 11) {
    X <- sum(sample(1:6, 2, replace = TRUE))
    if (collected[X] == 0) {
      collectedn <- collectedn + 1
    }
    collected[X] = 1
    trials <- trials + 1
  }
  trials
}

# Simulation
result <- replicate(n, f())
c( mean(result), var(result), min(result), max(result))
