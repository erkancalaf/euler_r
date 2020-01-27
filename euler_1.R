# Euler_1 Solution ------------------------------
# https://projecteuler.net/problem=1
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