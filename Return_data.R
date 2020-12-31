##The objective of this function is to create 
##      The return data matrix for calculations
##      Given the Rawdata

Return_data <- function(rawdata){
  len_tick <- ncol(rawdata)
  Nrows <- nrow(rawdata)
  
  Simple_Ret_matrix <- matrix(nrow = (Nrows-1),ncol = (len_tick))
  
  for (i in 1 : len_tick)
  {
    for (j in (1:(Nrows-1)))
    {
      Simple_Ret_matrix[j,i] <- (rawdata[(j+1),i]/rawdata[j,i]) - 1
    }
  }
  return(Simple_Ret_matrix)
}
