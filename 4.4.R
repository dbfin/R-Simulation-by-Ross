# Let H(n, k) be the number of permutations of n elements with k hits.
# To generate all permutations of size n+1 we start with a permutation
# of n elements, and first place n+1 at its position. Then we swap n+1
# with all other numbers. If the initial permutation of n elements had
# k hits, then the first permutation of n+1 elements has k+1 hits, and
# after swapping n+1 comes not to its position (-1 hit), and the other
# number that comes to position n+1 becomes also not a hit, so that if
# it was a hit before we have another -1 hit. Therefore,
#
# B(n+1, k) = B(n, k-1) + (n-k) B(n, k) + (k+1) B(n, k+1).
#
# The first few n (with the expectations and variances) are
#
# n\k  0  1  2  3  4  5  E  V
#  1   0  1              1  0
#  2   1  0  1           1  1
#  3   2  3  0  1        1  1
#  4   9  8  6  0  1     1  1
#  5  44 45 20 10  0  1  1  1
#
# The tendency is clear.

# We prove formally that if H is the number of hits in a permutation of
# n elements, then EH = 1 and VH = 0 if n = 1 or 1 if n > 1. Indeed, if
# n = 1, then with probability 1 we have 1 hit, so that EH = 1 and VH =
# 0. For n > 1 we can construct a permutation as follows. First, let P'
# be a random permutation of n-1 elements, and let X=k be the number of
# hits in P'. Second, we place the number n with probability 1/n at any
# position in P', and if there is already an element there we place the
# element at the position n. Let Y = H - X, i.e. Y is the change in the
# number of hits in the resulting permutation P compared to X. Then, we
# have that conditional on P', X = k with probability 1, and Y = 1 with
# probability 1/n, Y = -1 with probability k/n, Y = 0 otherwise. Hence,
# EH = E(E(X + Y|P')) = E(X + (1 - X) / n) = 1, because by induction we
# assume that EX = 1. Further, VH = E(V(H|P')) + V(E(H|P')). Therefore,
# VH = E(V(Y|P')) + V(X + E(Y|P')) = E( (X + 1) / n - (X - 1)^2 / n^2 )
# + V( X + (1 - X) / n ) = 2 / n + (n - 2) / n * VX. For n = 2, VH = 1.
# For n > 2, VX = 1, and VH = 1 as well.

# Simulations

# Sample size
ss <- 100000

# n
n <- 100

# A random permutation
P <- function (n) {
  sample(1:n, n)
}

# The number of hits in a permutation
hits <- function (P) {
  sum(P == 1:length(P))
}

result <- replicate(ss, hits(P(n)))
c( mean(result), var(result) )
