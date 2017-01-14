g4 <- function(n){
  x <- numeric(n)
  for(j in 1:n){
      u1 <- runif(1)
      u2 <- runif(1)
      if(u1 < 0.5){
        x[j] <- floor(10 * u2) + 1
      } else{
        x[j] <- floor(5 * u2) + 6
      }
  }
  x
}

z <- g4(5000) #changed to 5000

mean(z == 3) #changed from 9 to 3
