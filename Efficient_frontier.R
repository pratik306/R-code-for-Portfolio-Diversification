##Objective
##  1. Calculation of the efficient frontier

Efficient_frontier <- function(Output_Global_Min, mu, cov_matrix,iterations_top,iterations_below){
  
  ###Initialising required vectors for calculation of the efficient frontier
  len_tick <- ncol(mu)
  
  #Creating "A" matrix
  eff_A <- matrix(nrow = len_tick+2, ncol = len_tick+2)
  eff_A[1:len_tick,1:len_tick] <- 2*cov_matrix
  eff_A[1:len_tick,len_tick+1] <- mu
  eff_A[1:len_tick,len_tick+2] <- 1
  eff_A[len_tick+1,1:len_tick] <- t(mu) 
  eff_A[len_tick+2,1:len_tick] <- 1
  eff_A[(len_tick+1):(len_tick+2),(len_tick+1):(len_tick+2)] <- 0
  ##Matrix A has been created
  
  
  ##"Creating b matrix"
  b_eff <- matrix(nrow = len_tick+2, ncol = 1)
  b_eff[1:len_tick,1] <- 0
  b_eff[len_tick+2,1] <- 1
  #b matrix has been created
  
  ##Creating Z matrix
  ##"Creating efficient frontier Z vector
  z_eff <- matrix(nrow = len_tick+2 , ncol = 1)
  ##Z matrix has been created
  
  ##Things that will need to be iterated --- mu_po---- i.e create a mupo matrix
  
  mu_po <- matrix(nrow = (iterations_top + iterations_below), ncol = 1)
  
  
  
  ##Creating b matrix
  
  correspondingSD <- matrix(nrow = (iterations_top + iterations_below),ncol = 1)
  top_deltaformu <- (max(mu)-Output_Global_Min[1])/iterations_top
  below_deltaformu <- (Output_Global_Min[1] - min(mu))/iterations_below
  
  for (i in 1:iterations_top)
  {
    if (i == 1){
      mu_po[1,1] <- Output_Global_Min[1]
      correspondingSD[1,1] <- Output_Global_Min[2,1]
    }
    else {
      mu_po [i,1] <-  Output_Global_Min[1] + ((i-1)*top_deltaformu) 
      b_eff[1+len_tick,1] <- mu_po[i,1]
      z_eff <- inv(eff_A)%*%b_eff
      correspondingSD[i,1] <- sqrt(t(z_eff[1:len_tick,1])%*%cov_matrix%*%(z_eff[1:len_tick,1]))
    }
  }
  
  for (i in (1+iterations_top):(iterations_top + iterations_below))
  {
    
    mu_po [i,1] <-  Output_Global_Min[1] - ((i-(iterations_top))*below_deltaformu) 
    b_eff[1+len_tick,1] <- mu_po[i,1]
    z_eff <- inv(eff_A)%*%b_eff
    correspondingSD[i,1] <- sqrt(t(z_eff[1:len_tick,1])%*%cov_matrix%*%(z_eff[1:len_tick,1]))
    
  }
  eff_front <- cbind(correspondingSD,mu_po)
  return(eff_front)
  ###This is where the function to calculate the efficient frontier ENDS
  ###Calculating the efficient frontier
  ##Efficient Frontier calculated
  
}