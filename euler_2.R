# 2. euler_2 ------------------------------
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
  retVal <- sum(temp[temp<x & temp %% 2 != 0])
  return(retVal)
}

sum(euler_2(4000000))
