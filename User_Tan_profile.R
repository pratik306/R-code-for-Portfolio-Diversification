#Objective
#   1. Create User profile on tanegent 
User_Tan_profile <- function(User_expectret,tickers,tan_matSD,tan_matret,risk_free_rate){
  
  len_tick <- length(tickers)
  User_expecttan_contri <- as.numeric((User_expectret-risk_free_rate)/(tan_matret - risk_free_rate))
  User_expectSD <- User_expecttan_contri*tan_matSD
  User_profile <- matrix(nrow = len_tick+1, ncol = 1)
  User_profile[(1:len_tick),1] <- User_expecttan_contri*(tan_mat)
  User_profile[len_tick+1,1] <- (1-User_expecttan_contri)
  rownames(User_profile) <- append(tickers,"T-Bills")
  Newlist <- list("UserProfile" = User_profile, "TanContri" = User_expecttan_contri)
  return(Newlist)
}