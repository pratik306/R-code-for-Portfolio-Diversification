##Objective
#   1. Calculating Value at Risk
#   Note: Probability level assumed to be 95%
Value_At_Risk <- function(Total_Invest_val,User_expectret,User_expecttan_contri,tan_matSD){
  ###Calculating Value at Risk for a 5% Probability
  
  Risk_quantile <- User_expectret + (User_expecttan_contri*as.numeric(tan_matSD)*(-1.645))
  
  #Asking user for Total Investment value
  Val_at_Risk <- Total_Invest_val*Risk_quantile
  return(Val_at_Risk)
  
}