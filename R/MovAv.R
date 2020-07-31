#### Moving average ----
# moving average with default window-width of 11 and centered. 
# edges of x are averaged over a shrinking window-width
MovAv <- function(x, w = 11, align = "center", edges = "fill"){
  
  # x vector of numerical values
  # w width of window
  # when w is even, include one more value from future
  # align = "center" average assigned to centre of window
  # align = "left" average of current value and next (w-1) values 
  # align = "right" average of current value and previous (w-1) values 
  # edges = "NA" fill with NA when window does not include w values
  # edges = "fill" average of reducing number of values at the edges of the vector
  
  if (align == "center") {
    before <- floor((w-1)/2)
    after  <- ceiling((w-1)/2)
  } else if (align == "right") {
    before <- w-1
    after  <- 0
  } else if (align == "left") {
    before <- 0
    after  <- w-1
  } else {
    print("'align' should be 'center', 'left' or 'right'")
  }
  
  run_mean <- matrix(NA, nrow = length(x), ncol = 1)
  n <- length(x)
  if (edges == "fill") {
    
    for (i in 1:n) {
      if(is.na(x[i]) == FALSE) {
        run_mean[i] <- mean(x[max(0, (i-before)):(i+after)], na.rm = TRUE) }
      else if(is.na(x[i]) == TRUE) {
        run_mean[i] <- NA }
      else{
        print("Error in dataframe. Missing values?") }
    }
    
  } else if (edges == "NA"){
    
    for (i in 1:n) {
      # when x start with non-NA 
      if (i-before <= 0) {run_mean[i] <- NA
      } else {
        run_mean[i] <- mean(x[max(0, (i-before)):(i+after)], na.rm = FALSE) }
    }
    
  } else {
    
    print("Take care of the edges! 'Edges' should be 'NA' or 'fill'.") 
  }
  
  return(run_mean)
}