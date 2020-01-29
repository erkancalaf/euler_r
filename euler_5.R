euler_5 <- function(x){
  start_num <- 1
  
  while (start_num==start_num) {
    remain_sum = 0
    for (i in 1:x) {
      remain <- start_num %% i
      remain_sum <- remain_sum + remain
    }  
    if (remain_sum == 0) {
      return(start_num)
      break
    } else {
      start_num = start_num+1  
    }
  }
}


euler_5(20)