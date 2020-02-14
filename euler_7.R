euler_7 <- function(n){
  temp_prime <- c(2)
  temp_num <- 3
  
  while (length(temp_prime) < n) {
    if (
      # if is prime
      if (temp_num != 2) {
      all(temp_num %% 2:(temp_num-1) != 0)}
      ){
      # temp_prime'a ekle
      temp_prime <- c(temp_prime, temp_num)
    }
    temp_num = temp_num + 1 
  }
  return(temp_prime[length(temp_prime)])
}
