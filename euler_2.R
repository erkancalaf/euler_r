# Euler_2 Solution ------------------------------
# https://projecteuler.net/problem=2
euler_2 <- function(x){
  num_1 <- 1
  num_2 <- 2
  temp <- c(num_1, num_2)
  result <- 0
  
  while (result < x) {
    result <- num_1 + num_2
    num_1 <- num_2
    num_2 <- result
    temp <- c(temp, result)
  }
  ret_val <- sum(temp[temp < x & temp %% 2 != 0])
  return(ret_val)
}

euler_2(4000000)
