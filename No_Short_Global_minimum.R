#Objective
#Finding No short Global Minimum
No_Short_Global_minimum <- function(cov_matrix,mu){
  
  len_tick <- ncol(mu)
  
  small_dvec_forquadprgo <- rep(0, len_tick)
  
  ##Initialising matrices for calculations
  A_for_Noshort_globmin <- matrix(ncol = (len_tick+1),nrow = len_tick)
  B_for_Noshort_globmin <- matrix(ncol = (len_tick+1), nrow = 1)
  
  
  ##Assigning values
  Ident_matr <- matrix(data = 0, nrow = len_tick, ncol = len_tick)
  diag(Ident_matr) <- 1
  
  A_for_Noshort_globmin[1:len_tick,1] <- 1
  A_for_Noshort_globmin[1:len_tick, 2:(len_tick+1) ] <- Ident_matr
  
  
  B_for_Noshort_globmin[1,1] <- 1
  B_for_Noshort_globmin[1, 2:(len_tick+1)] <- 0
  
  
  
  ##Solving for no short profile
  qp.out.globmin <- solve.QP(Dmat = (2*cov_matrix), dvec = (small_dvec_forquadprgo), Amat = A_for_Noshort_globmin, bvec = B_for_Noshort_globmin, meq = 1)
  globmin_noshort_Stddev <- sqrt(as.numeric(qp.out.globmin$value))
  qp.out.globmin_Weight <- as.matrix(qp.out.globmin$solution)
  Expect_return_globmin_Noshort <- mu%*%qp.out.globmin_Weight
  
  Newlist <- list("Return" = Expect_return_globmin_Noshort  ,"Risk" = globmin_noshort_Stddev, "Weights" = qp.out.globmin_Weight)
  return(Newlist)
  #ggplot(Data_forplot, aes(correspondingSD,mu_po)) + geom_line() + geom_abline(intercept = 0, slope = (as.numeric(tan_matret)/as.numeric(tan_matSD)),color = "red",linetype = "dashed" ) + geom_point(aes(Std_ret_Noshort,User_expectret_Noshort), colour = "blue") + geom_point(aes(globmin_noshort_Stddev,Expect_return_globmin_Noshort), colour = "purple") + geom_point(aes(glob_min_SD,Expectedreturn_Glob_Min), colour = "red")
  
}