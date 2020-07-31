#### Summed probability density ----
# computes a data frame with the SPD based on x = c(keycode, Date_end, SWR, Waneyedge)

sapwood_SPD <- function(x, scale_p = FALSE, run_mean = TRUE, w = 11){
  # computes the summed probability density of a series distributions
  
  x <- as.data.frame(x)
  timeRange <- range(x[, 2])
  timeAxis <- seq(timeRange[1], timeRange[2] + 100, by = 1)
  
  PDF_matrix <- matrix(nrow = length(timeAxis), ncol = 1)
  keycodes <- x[, 1]
  keycodes <- as.character(keycodes)
  endDate <- x[, 2]
  swr <- x[, 3]
  cambium <- x[, 4]
  
  if (any(is.na(as.numeric(endDate))) | is.factor(endDate) | is.logical(endDate))
  {stop(" --> input does not provide end dates in the second column (some are empty or not numeric)")}
  if (is.character(swr)) stop(" --> input has non-numeric swr-numbers in the third column")
  if (!is.logical(cambium)) stop(" --> fourth column of input data.frame should be logical (TRUE/FALSE), indicating the presence of waney edge")
  
  
  PDF_matrix[,1] <- timeAxis
  colnames(PDF_matrix) <- "year"
  
  for (i in 1:length(keycodes)){
    keycode_i <- keycodes[i]
    end_i <- endDate[i]
    swr_i <- swr[i]
    yr <- endDate[i]
    cambium_i <- cambium[i]
    
    if (cambium_i == TRUE){
      fellingDate <- x[i, 2]
      pdf <- matrix(NA, nrow = 1, ncol = 2)
      colnames(pdf) <- c("year", keycode_i)
      pdf[1,1] <- fellingDate
      pdf[1,2] <- 1
      PDF_matrix <- merge(PDF_matrix, pdf, by = "year", all = TRUE)
    } else {
      pdf <- sapwood_PDF(swr_i, end_i)
      pdf <- pdf[, -2]
      # pdf_years <- seq(yr, yr + nrow(pdf) -1, by = 1)
      # pdf[, 1] <- pdf_years
      colnames(pdf) <- c("year", keycode_i)
      PDF_matrix <- merge(PDF_matrix, pdf, by = "year", all = TRUE)
    }
  }
  
  # sum probabilities to SPD
  PDF_matrix$SPD <- rowSums(PDF_matrix[, -1], na.rm = TRUE) 
  
  # scale SPD to 1
  if (scale_p == TRUE){
  PDF_matrix$SPD <- PDF_matrix$SPD/sum(PDF_matrix$SPD, na.rm = TRUE)
  } 
  
  if (run_mean == TRUE) {
  # add 11-year running mean (default)
  PDF_matrix$SPD_MovAv <- MovAv(PDF_matrix$SPD, w = w, align = "center", edge = "NA")
  } 
  
  return(PDF_matrix)
}
