# The number of points
while (TRUE) {
  n <- as.integer(readline(prompt = "n: "))
  if (!is.na(n) && n > 0) break;
}

# The probabilities
p <- 1:n
sum <- 0
i <- 1
while (i < n) {
  while (TRUE) {
    cat("The current sum of probabilities is", sum, "\n")
    cat("Probability", i)
    pcur <- as.double(readline(prompt = ""))
    if (!is.na(pcur) && pcur > 0 && sum + pcur < 1) break;
  }
  p[i] <- pcur
  sum <- sum + pcur
  i <- i + 1
}
p[n] <- 1.0 - sum

# Sample function
X_sample <- function (size, n, p) {
  sample(1:n, size, replace = TRUE, prob = p)
}

# Probabilities
cat("\nThe probabilities are", p)

# Frequencies
size <- 100000 * n
prop.table(table(X_sample(size, n, p)))
