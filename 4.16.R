# Since the 5 lowest probabilities are all 0.06, we can create 5 random variables
# with the corresponding probabilities of 1, 2, 3, 4 and 5 being 0.06 * 9 = 0.54,
# then the corresponding probabilities of their alternatives are 0.46, therefore,
# 0.46 / 9 = 0.05111... is their contribution to the outcomes from 6 to 10. Among
# these values, 0.13 is the lowest probability corresponding to two more numbers.
# Hence, we create two more random variables with the probabilities of 7 and 10 =
# (0.13 - 2 * 0.46 / 9) * 9 = 0.25, where the probabilities of the other option =
# 0.75. This contributes 0.75 / 9 to other alternatives. One more 0.46 we can use
# for 8. We create another random variable with the probability of the outcome 14
# = (0.14 - 0.46 / 9) * 9 = 0.8, and the other probability = 0.2.

# Basically, we split all probabilities * 9 among pairs of probabilities:
#    1    2    3    4    5    6    7    8    9   10
# 0.54 0.54 0.54 0.54 0.54 1.35 1.17 1.26 1.35 1.17
# 0.54                          0.46
#      0.54                     0.46
#           0.54                               0.46
#                0.54                          0.46
#                     0.54           0.46
#                                    0.80 0.20
#                          0.75 0.25
#                                         0.75 0.25
#                          0.60           0.40
# I also make in each case the probability of the lower option larger than
# than the probability of the higher option for convenience

n = 1000000

outcomes = matrix(c(1, 7, 2, 7, 3, 10, 4, 10, 5, 8, 8, 9, 6, 7, 9, 10, 6, 9),
                  nrow = 2)
probabilities = c(0.54, 0.54, 0.54, 0.54, 0.54, 0.80, 0.75, 0.75, 0.60)

U <- runif(n) * 9 + 1
s <- floor(U)
s <- outcomes[cbind(floor((U - s) / probabilities[s]) + 1, s)]

prop.table(table(s))
