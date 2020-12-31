###Function to create a logarithmic return matrix
log_Ret_Data <- function(rawdata)
{
  len_tick <- ncol(rawdata)
  Nrows <- nrow(rawdata)
  
  log_Ret_matrix <- matrix(nrow = (Nrows-1),ncol = (len_tick))
  
  for (i in 1 : len_tick)
  {
    for (j in (1:(Nrows-1)))
    {
      log_Ret_matrix[j,i] <- log(rawdata[(j+1),i]/rawdata[j,i])
    }
  }
  return(log_Ret_matrix)
}