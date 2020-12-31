##Objective
##Creating a vector of mean returns of a stock

mean_ret <- function(log_Ret_matrix){
  
  len_tick <- ncol(log_Ret_matrix)
  
  mu <- (matrix(nrow = 1, ncol = len_tick))
  
  
  for (i in 1 : len_tick)
  {
    
    mu[i] = mean(log_Ret_matrix[,i])
  
  }
 
   return(mu)
}