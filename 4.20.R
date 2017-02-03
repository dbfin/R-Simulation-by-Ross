# This is just the acceptance-rejection method using qj = P{X = j}, and
# c = 1/P{Y = j}. Indeed, then after generating X we accept it with the
# probability P{X = i|Y = j}/((1/P{Y = j})P{X = i}) = P{Y = j|X = i}.

# I think this method is also illustrative, because we can further modify it
# as follows. Suppose that neither P{X = i|Y = j} nor P{Y = j|X = i} is easy
# to calculate. Also, suppose that it is not easy to simulate {X|Y = j}, but
# it is relatively easy to simulate {Y| X = i}. Then, we can use the method,
# where instead of generating U and comparing it to P{Y = j|X = i}, which is
# difficult to calculate, we simply simulate {Y|X = i}, and then accept X if
# the resulting Y = j, that is exactly with probability P{Y = j|X = i}.

# Suppose we roll a dice, and set X as the resulting number from 1 to 6. Then,
# we roll the dice X times, and take Y as the sum of the results. In this case
# both P{Y = j|X = i} and P{X = i|Y = j} are difficult to calculate, but {Y|X}
# is easy to simulate. Hence, we can use the method to sample X conditional on
# Y = j. For example, the result for j = 2 should be X = 1 (6/7), 2 (1/7), and
# for j = 3, X = 1 (36/49), 2 (12/49), 3 (1/49). What about j = 12?

n = 10000

X <- function (j) {
  while (TRUE) {
    X <- floor(runif(1) * 6) + 1
    if (sum(sample(1:6, X, replace = TRUE)) == j) {
      break
    }
  }
  X
}

rbind(c( 6/7, 1/7 ), prop.table(table(replicate(n, X(2)))))
rbind(c( 36/49, 12/49, 1/49 ), prop.table(table(replicate(n, X(3)))))
rbind(rep("?", 5), prop.table(table(replicate(n, X(12)))))
