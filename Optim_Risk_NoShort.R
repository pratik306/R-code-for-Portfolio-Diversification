##Objective
##To find out optimal risky portfolio for no short restriction
Optim_Risk_NoShort <- function(iterations,glob_min_noshort_ret,maxret_stock,mu,cov_matrix,risk_free_rate){
  Std_data <- matrix(nrow = iterations, ncol = 1)
  Ret_data <- matrix(nrow = iterations, ncol = 1)
  Corresponding_weight <- matrix(nrow = iterations, ncol = ncol(mu))
  OptimPF <- matrix(nrow = ncol(mu)+2, ncol = 1)
  Ret_delta <- (maxret_stock-glob_min_noshort_ret)/(iterations)
  maxsharpematrix <- matrix(nrow = iterations,ncol = 1)
  for (i in 1:iterations){
   
    retcalc <- glob_min_noshort_ret + i* Ret_delta
    Ret_data[i,1] <- retcalc
    temp <- No_Short_User(retcalc,mu,cov_matrix)
    Std_data[i,1] <- temp$Risk
    Corresponding_weight[i,1:ncol(mu)] <-  temp$Weights
  }
  for (j in 1:iterations){
  maxsharpematrix[j,1] <- ((Ret_data[j,1] - risk_free_rate)/Std_data[j,1])
  }
  maxsharpe <- max(maxsharpematrix)
  reqindex <- match(maxsharpe, maxsharpematrix)
  OptimriskPF_Ret <- Ret_data[reqindex,1]
  OptimriskPF_Std <- Std_data[reqindex,1]
  OptimriskPF_weight <- Corresponding_weight[reqindex,1:length(mu)]
  ultralist <- list("weights" = Corresponding_weight,"Returndata" = Ret_data, "Riskdata"= Std_data,"Optimret" = OptimriskPF_Ret,"OptimStd" = OptimriskPF_Std,"Optimweight" = OptimriskPF_weight)
  return(ultralist)
}