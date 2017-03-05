# The problem is not easy to solve theoretically. First, directly, the number
# of claims is distributed as B(n, p), where n is the number of policyholders
# and p is the probability of a claim. The total amount claimed is then equal
# to Gamma(B(n, p), l), where 1 / l is the mean amount claimed by each claim,
# i.e. l = 1 / 800. The density is
# f(x) = sum[k=0,n] C(n, k) p^k (1-p)^(n-k) l e^(-l x) (l x)^(k - 1) / (k-1)!
# I am not sure how to further deal with this expression.

# Alternatively, suppose each policyholder claims an amount ~ Exp(l), and the
# insurance company picks claims to approve at random, independently with the
# probability p for each claim. Then, clearly the expected amount the company
# has to pay is EM = n * p / l = 40000. So, if the distribution of the amount
# is not skewed too much, we expect P(M > 50000) to be below 0.5. In fact, it
# can be well below 0.5, because Var(M) = E(Var(M | C)) + Var(E(M | C)) where
# C is the number of claims paid to clients. E(Var(M | C)) = E(C / l^2) = n *
# p / l^2, and Var(E(M | C)) = Var(C / l) = n * p * (1 - p) / l^2. So, Var(M)
# = n * p * (2 - p) / l^2. Thus, P(M - EM > a) < 1 / (1 + a^2 / Var(M)) = 1 /
# (1 + a^2 * l^2 / (n * p * (2-p))). For the given n, p and l, and a = 10000,
# P(M - EM > a) < 1 / (1 + 10^8 / 800^2 / (50 * 1.95)) = 0.38. Note, Var(M) =
# 62400000 = 6.24 * 10^7.

n <- 1000
p <- 0.05
l <- 1 / 800

N <- 100000

# Using the gamma distribution
s1 <- rgamma(N, rbinom(N, n, p), l)

# Direct simulation
s2 <- replicate(N, sum(sample(0:1, n, TRUE, c(1 - p, p)) * rexp(n, l)))


c(length(which(s1 > 50000)) / N, mean(s1), var(s1),
  quantile(s1, c(0.25, 0.5, 0.75)))
c(length(which(s2 > 50000)) / N, mean(s2), var(s2),
  quantile(s2, c(0.25, 0.5, 0.75)))
