#### sapwood_PDF function ----

sapwood_PDF <- function(swr = NA, last = 0, hdi = FALSE, credMass = 0.954, model = "Hollstein_1980"){ 
  # This function computes the probability density function (PDF)
  # and highest probability density interval (hdi) based on sapwood models.
  
  # swr = number of sapwood rings observed/measured
  # last = calendar year during which the outermost sapwood ring was formed (optional, default = 0)
  # hdi = If TRUE the lower and upper limit of the highest density interval (credible interval = p) is given. 
  #       If FALSE a matrix is returned with scaled p values for each number of observed sapwood rings.
  # credMass = value to set the credible interval, value between 0 and 1.
  # model = one of c("Hollstein_1980", "Wazny_1990", ...) --> add more (Sohar, Haneca, Heussner, ...)

  # Check if package "HDInterval" is loaded. Install if necessary.  
  if (hdi == TRUE) {
    
    if (!require(HDInterval)) {install.packages("HDInterval")} else {library(HDInterval)}
  }
  # check input --> x should be an integer
  if (!is.numeric(swr) & !is.na(swr)) stop(" --> swr must be a numeric value")
  if (swr < 0 & !is.na(swr)) stop(" --> swr must be a positive number")
  if (isTRUE(swr%%1 != 0)) stop(" --> swr must be an integer (no decimals allowed!)")
  # When no x or non-numeric is supplied to the function --> return NA
  if (is.na(swr)) #warning(" --> swr = NA")
                  return(NA_integer_)
    
  # swr = vector of lenght 101 with sequence 1:100 (increasing no. of sapwood rings) in first column
  swr_seq <- seq(swr, swr + 100, by = 1)
  # years = sequence of calendar years, starting from last dated tree-ring (length = 101)
  years <- seq(last, last + 100, by = 1)
  # initiate an empty matrix for storing output
  pdf_matrix <- matrix(NA, nrow = length(swr_seq), ncol = 3)
  pdf_matrix[, 1] <- years
  pdf_matrix[, 2] <- swr_seq
  colnames(pdf_matrix) <- c("year", "swr", "p")
  
  if (model == "Wazny_1990") {
    # Wazny 1990 sapwood model
    a = 2.75290
    sigma = 0.293951
    # lognormal distribution describing the sapwood model
    pdf_matrix[,3] <- dlnorm(seq(swr, swr + 100, 1), meanlog = a, sdlog = sigma)
  } else {
    # Hollstein 1980 sapwood model
    a = 2.81357890
    sigma = 0.41620736
    # lognormal distribution describing the sapwood model
    pdf_matrix[,3] <- dlnorm(seq(swr, swr + 100, 1), meanlog = a, sdlog = sigma)
  }
    
  pdf_matrix <- as.data.frame(pdf_matrix)
  # filter extreme low p values
  pdf_matrix <- subset(pdf_matrix, p > 0.000001 | swr < 5)
  pdf_matrix$p[pdf_matrix$p < 0.000001] <- 0
  # scale density function to 1
  pdf_matrix$p <-  pdf_matrix$p/sum(pdf_matrix$p)
    pdf_matrix
  
  if (hdi == FALSE){
    
    return(pdf_matrix)
    
  } else if (nrow(pdf_matrix) == 0) {
    # when a very high number of swr is given, the pdf_matrix is empty --> create hdi manually
    hdi <- c(last, NA_integer_)
    attributes(hdi)$names <- c("lower", "upper")
    attributes(hdi)$credMass <- credMass
    attributes(hdi)$sapwood_model <- model
      return(hdi)
    
  } else {
    # take a large sample from the distribution
    sample <- sample(pdf_matrix$swr, size = 100000, replace = TRUE, prob = pdf_matrix$p)
    sample <- as.data.frame(sample)
    hdi <- HDInterval::hdi(sample$sample, credMass = credMass, allowSplit = TRUE)
    # Add calendar years to output when y is provided
    if (last == 0) {
      
      attributes(hdi)$sapwood_model <- model
      return(hdi)
      
    } else {
      
      hdi[1] <- hdi[1] - swr + last
      hdi[2] <- hdi[2] - swr + last 
      attributes(hdi)$sapwood_model <- model
      
      return(hdi)
    
  }
 }
}
