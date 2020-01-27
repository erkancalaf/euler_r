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


# 4. euler_4 ------------------------------
euler_4 <- function(x){
  lower_bound <- 10^x
  upper_bound <- lower_bound*10-1
  ret_val <- c()
  for (i in lower_bound:upper_bound) {
    for (j in lower_bound:upper_bound) {
      temp = i*j
      reverse <- 0
      temp_ <- temp
      while (temp_ > 0) {
        reverse <- reverse*10 + temp_%%10
        temp_ = floor(temp_/10)
      }
      if (temp == reverse) {
        ret_val <- c(ret_val, temp)
      }
    }
  }
  return(max(ret_val))
}


euler_4(2)
