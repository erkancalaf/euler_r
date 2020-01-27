# Euler_3 Solution  ------------------------------
# https://projecteuler.net/problem=3
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
  return(max(result_factor))
}

euler_3(600851475143 )