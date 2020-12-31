##Objective
##    1. To calculate tangency Frontier
Tangency_Portfolio <- function(risk_free_rate,cov_matrix,mu,efficient_frontier){
  
  
  len_tick = ncol(mu)
  one <- (matrix(data = 1, nrow = 1, ncol = len_tick))
  correspondingSD <- efficient_frontier[,1]
  mu_po <- efficient_frontier[,2]
  
  
  ##Initializing variables
  tan_mat <- matrix(nrow = len_tick, ncol = 1)
  tan_mat <- (inv(cov_matrix)%*%(t(mu) - (risk_free_rate*t(one))))/as.numeric(one%*%inv(cov_matrix)%*%(t(mu) - risk_free_rate*t(one)))
  tan_matret <- (t(tan_mat)%*%t(mu))           
  tan_matSD <- (sqrt(t(tan_mat)%*%cov_matrix%*%tan_mat))
  
  newlist <- list("TanSD" = tan_matSD, "TanRet" = tan_matret, "TanMat" = tan_mat)
  return(newlist)
  ##Check plot
  #(ggplot(Data_forplot, aes(correspondingSD,mu_po)) + geom_line() + geom_abline(intercept = 0, slope = (as.numeric(tan_matret)/as.numeric(tan_matSD)),color = "red",linetype = "dashed" ))
  
  }