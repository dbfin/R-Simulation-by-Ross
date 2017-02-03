# This is similar to an example in the text

n = 1000000

# We can think of it as follows:
# the pairs (5, 6), ..., (13, 14) each has probability 1/5,
# and then within the pair we determine the element with 11/20:9/20 chances

U <- runif(n) * 5
s <- floor(U)
s <- s * 2 + 5 + floor((U - s) * 20 / 11)

prop.table(table(s))
