##Objective
##  1. To create a covariance matrix

covariance <- function(log_Ret_matrix, mu){
  len_tick <- ncol(log_Ret_matrix)
  Nrows <- nrow(log_Ret_matrix)
  
  diff <- (matrix(nrow = Nrows, ncol = len_tick))
  
  for (i in 1:len_tick)
  {
    for (j in 1:(Nrows))
    {
      diff[j,i] <- log_Ret_matrix[j,i] - mu[1,i]
    }
  }
  
  cov_matrix <- (matrix (nrow = len_tick, ncol = len_tick))
  cov_matrix <- ((nrow(log_Ret_matrix)^-1)* t(diff)%*%(diff))
  
  return(cov_matrix)
}