euler_6 <- function(x){
  sum_square <- 0
  sum <- 0
  
  for (i in 1:x) {
    sum_square = i^2 + sum_square
    sum <- sum + i
  }
  square_sum <- sum^2
  diff <- square_sum - sum_square
  
  return(diff)
}

euler_6(100)
