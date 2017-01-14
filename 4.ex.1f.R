#Example 4f.


f4 <- function(n){
  
  p <- c(0.11, 0.12, 0.09, 0.08, 0.12, 0.1, 0.09, 0.09, 0.1, 0.1)
  x <- numeric(n)
  
  for(i in 1:n){
    while(TRUE){
    u1 <- runif(1)
    y <- floor(10 * u1) + 1
    u2 <- runif(1)
    if(u2 <= p[y] / .12){
      x[i] <- y
    break }
    }
  }
  x
}

z <- f4(10000)
mean(z == 5)
