##The objective of this function is to
##          1. Extract the data from the internet 
##          2. Tidy the data
##          3. Return data on which further computations can be done
Extract_Data <- function(tickers,Dt_start,Dt_end,type){
 len_tick <- length(tickers)
  
###Finding out how many rows will be present in our data
  prices_Test <- (get.hist.quote(instrument = tickers[1],start = Dt_start, end = Dt_end, quote = "Adjusted", provider = "yahoo", compression = type ))
  prices_Test <- as.matrix(prices_Test)
  
  Nrows_matrix <- nrow(prices_Test)
  Ncol_matrix <- len_tick
  myrawdata <- (matrix(nrow = Nrows_matrix, ncol = Ncol_matrix))
  
  for (i in 1:len_tick)
  {
    
    myrawdata[, i] = get.hist.quote(instrument = tickers[i],start = Dt_start, end = Dt_end, quote = "Adjusted", provider = "yahoo", compression = type )
  }
  
  colnames(myrawdata) <- tickers
  return(myrawdata)
}
