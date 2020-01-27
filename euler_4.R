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