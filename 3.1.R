# The initial value of the seed (x0 in the exercise)
seed <<- 5

# A function that evaluates the next value of the seed and returns it
new_random <- function () {
  seed <<- 3 * seed %% 150
} 

# Calculate 10 random values
result <- replicate(10, new_random())
result
