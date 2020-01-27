# 1. euler_1 ------------------------------
euler_1 <- function(x){
  j = 0
  for (i in 1:x-1) {
    if (i %% 3 == 0 | i %% 5 == 0) {
      j = i + j
    }
  }  
  return(j)
}

euler_1(1000)

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

# 3. euler_3 ------------------------------
euler_3 <- function(x){
  result_factor <- c()
  i = 2
  while (x!=1) {
    while (x %% i == 0) {
      x = x/i
      result_factor <- c(result_factor,i)
    }
    i = i+1
  }
  return(result_factor)
}

max(euler_3(600851475143 ))


