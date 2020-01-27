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