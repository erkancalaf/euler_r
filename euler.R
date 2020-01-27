# 1. euler_1 ------------------------------
euler_1 <- function(x){
  j = 0
  for (i in 1:x-1) {
    if (i %% 2 == 0 | i %% 7 == 0) {
      j = i + j
    }
  }  
  return(j)
}