##Objective
##      1. Create a Global minimum variance portfolio


Global_Minimum_Variance <- function(mu, cov_matrix,tickers){
  len_tick <- ncol(mu)
  globalmin_A <- matrix(nrow = len_tick+1, ncol = len_tick+1)
  
  #Setting values in the globalmin_A
  
  globalmin_A[1:len_tick,1:len_tick] <- 2*cov_matrix
  globalmin_A[1:len_tick,len_tick+1] <- 1
  globalmin_A[len_tick+1,1:len_tick] <- 1
  globalmin_A[len_tick+1, len_tick+1] <- 0
  
  ####Creating "Z" vector for minimum variance portfolio
  globalminZ <- matrix(nrow = len_tick+1, ncol = 1)
  globalminZ[1:len_tick , 1] <- 1/(len_tick)
  globalminZ[len_tick+1 , 1] = 0
  
  
  ###Creating "b" Vector for minimum variance portfolio
  globalminb <- matrix(nrow = len_tick+1, ncol = 1)
  globalminb[1:len_tick , 1] <- 0
  globalminb[len_tick+1 , 1] <- 1
  
  ###Calculating "Z" for the minimum variance frontier
  globalminZ  <- inv(globalmin_A)%*%globalminb
  
  
  ###Outputs of the global min variance Frontier
  #1.Expected Return
  #2.standard deviation
  #3. Weights of the portfolio
  
  Expectedreturn_Glob_Min <- mu%*%globalminZ[1:len_tick,1]
  glob_min_weights <- as.matrix(globalminZ[1:len_tick])
  glob_min_SD <- sqrt(t(glob_min_weights)%*%cov_matrix%*%(glob_min_weights))
  Output_Global_Min <- matrix(nrow = len_tick+2, ncol = 1)
  tempvar <- c("Return", "SD")
  rownames(Output_Global_Min) <- append(tempvar, tickers)
  Output_Global_Min[1,1] <- Expectedreturn_Glob_Min
  Output_Global_Min[2,1] <- glob_min_SD
  Output_Global_Min[3:(len_tick+2)] <- glob_min_weights
  
  return(Output_Global_Min)
  
}