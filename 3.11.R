# The sample size
n <- 1000000

# Corr(U, sqrt(1-U^2)) = (E(U sqrt(1-U^2)) - EU Esqrt(1-U^2)) /
#                        sqrt(( EU^2 - (EU)^2 )( E(1-U^2) - (Esqrt(1-U^2))^2 ))
#                      = -0.921384

# Corr(U^2, sqrt(1-U^2)) = (E(U^2 sqrt(1-U^2)) - EU^2 Esqrt(1-U^2)) /
#                          sqrt(( EU^4 - (EU^2)^2 )( E(1-U^2) - (Esqrt(1-U^2))^2 ))
#                      = -0.983555

# Monte Carlo
x <- runif(n)
sx <- sqrt(1 - x^2)
result <- c( (mean(x * sx) - mean(x) * mean(sx))
             / sqrt( mean((x-mean(x))^2) * mean((sx-mean(sx))^2) ),
             (mean(x^2 * sx) - mean(x^2) * mean(sx))
             / sqrt( mean((x^2-mean(x^2))^2) * mean((sx-mean(sx))^2) ) )
result
