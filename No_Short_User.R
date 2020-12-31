#Objective
#     1. Calculating User Profile for No short
No_Short_User <- function(User_expectret_Noshort,mu,cov_matrix){
  
  len_tick <- ncol(mu)
  
  ##Initialising matrices for calculations
  A_for_Noshort <- matrix(ncol = (len_tick+2),nrow = len_tick)
  B_for_Noshort <- matrix(ncol = (len_tick+2), nrow = 1)
  Ident_matr <- matrix(data = 0, nrow = len_tick, ncol = len_tick)
  diag(Ident_matr) <- 1
  small_dvec_forquadprgo <- rep(0, len_tick)
  
  
  ##Assigning values
  A_for_Noshort[1:len_tick,1] <-mu
  A_for_Noshort[1:len_tick,2] <- 1
  A_for_Noshort[1:len_tick, 3:(len_tick+2) ] <- Ident_matr
  
  B_for_Noshort[1,1] <- User_expectret_Noshort
  B_for_Noshort[1,2] <- 1
  B_for_Noshort[1, 3:(len_tick+2)] <- 0
  
  
  ##Solving for no short profile
  qp.out <- solve.QP(Dmat = (2*cov_matrix), dvec = (small_dvec_forquadprgo), Amat = A_for_Noshort, bvec = B_for_Noshort, meq = 2)
  cat("Porfolio weights for No short for - ", User_expectret_Noshort, "Return - are - ", qp.out$solution, "- With a STd Deviation of -", sqrt(qp.out$value) )
  
  Std_ret_Noshort <- sqrt(as.numeric(qp.out$value))
  
  Newlist <- list("Risk" = Std_ret_Noshort, "Weights" = qp.out$solution)
  return(Newlist)

  
}