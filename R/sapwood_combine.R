#### sapwood_combine function ----


sapwood_combine <- function(x, credMass = 0.954, hdi = FALSE, model = "Hollstein_1980"){
  
  # returns a data frame with the combined probability, based on x = c(keycode, Date_end, swr, Waneyedge)
  # combines multiple estimates for the feling date of a single event
  
  # input --> data.frame with four columns: x = c(keycode, Date_end, SWR, waneyedg)
  # first column must be is.character()
  # second and third column must be is.numeric() & integer x%%1 == 0
  # model = one of c("Hollstein_1980", "Wazny_1990", ...) --> argument for function sapwood_PDF()
  

  x <- as.data.frame(x)
  n <- nrow(x)
  endDate <- x[, 2]
  if (any(is.na(as.numeric(endDate))) | is.factor(endDate) | is.logical(endDate))
    {stop(" --> input does not provide end dates in the second column (some are empty or not numeric)")}

  swr <- x[, 3]
  if (is.character(swr)) stop(" --> input has non-numeric swr-numbers in the third column")
  
  cambium <- x[, 4]
  if (!is.logical(cambium)) stop(" --> fourth column of input data.frame should be logical (TRUE/FALSE), indicating the presence of waney edge")
  
  
  timeRange <- range(x[, 2])
  timeAxis <- seq(timeRange[1]-3, timeRange[2] + 100, by = 1)
  
  pdf_matrix <- matrix(nrow = length(timeAxis), ncol = 1)
  colnames(pdf_matrix) <- "year"
  pdf_matrix[, 1] <- timeAxis
  keycodes <- x[, 1]
  keycodes <- as.character(keycodes)
  
  # no series with sapwood or waney edge --> compute terminus post quem
  if (all(cambium == FALSE) & all(is.na(swr))) {
   
    last <- max(endDate)
    tpq_min <- sapwood_PDF(0, hdi = TRUE, credMass = credMass, model = model)[1]
    tpq <- last + tpq_min
    
    message <- paste0("earliest possible felling date: ", tpq)
    
    for (i in 1:length(keycodes)){
      keycode_i <- keycodes[i]
      pdf <- matrix(NA, nrow = length(timeAxis), ncol = 2)
      pdf[, 1] <- timeAxis
      colnames(pdf) <- c("year", keycode_i)
      yr <- endDate[i]
      pdf[pdf[, "year"] < yr, keycode_i] <- 0
      pdf[pdf[, "year"] >= yr, keycode_i] <- NA
      pdf_matrix <- merge(pdf_matrix, pdf, by = "year", all = TRUE)
    }
    # result of prod(c(NA,NA), na.rm TRUE) == 1
    # first remove rows with all NA (except "year")
    pdf_matrix <- pdf_matrix[rowSums(is.na(pdf_matrix)) != ncol(pdf_matrix)-1, ]
    
    # combine probabilities into one probability density function
    if (n == 1){
      
      pdf_matrix$COMB <- pdf_matrix[, 2]
      
    } else {
      
      pdf_matrix$COMB <- apply(pdf_matrix[, -1], 1, FUN=function(x) prod(x, na.rm = TRUE))
    }
    
    if (hdi == FALSE) {
      return(pdf_matrix)
      
    } else {
    hdi <- c(tpq, NA)
    attributes(hdi)$credMass <- credMass
    attributes(hdi)$sapwood_model <- model
    
    summary <- cbind(keycodes, endDate, swr, cambium, NA)
    colnames(summary) <- c("series", "endDate", "swr", "waneyEdge", "A_i")
    
    model_summary <- list(rawData = pdf_matrix,
                          A_c = NA,
                          A_comb = NA,
                          hdi_model = hdi,
                          summary = summary,
                          message = message)
    return(model_summary)
    }
    
  } else {
    
    pdf_matrix[, 1] <- timeAxis
    colnames(pdf_matrix) <- "year"
    
    for (i in 1:length(keycodes)){
      keycode_i <- keycodes[i]
      swr_i <- swr[i]
      yr <- endDate[i]
      cambium_i <- cambium[i]
      
      if (cambium_i == TRUE){
        pdf <- matrix(NA, nrow = length(timeAxis), ncol = 2)
        pdf[, 1] <- timeAxis
        colnames(pdf) <- c("year", keycode_i)
        pdf[pdf[, "year"] == yr, keycode_i] <- 1
        pdf[pdf[, "year"] != yr, keycode_i] <- 0
        pdf_matrix <- merge(pdf_matrix, pdf, by = "year", all = TRUE)
      
      } else if (is.na(swr_i)) {
        pdf <- matrix(NA, nrow = length(timeAxis), ncol = 2)
        pdf[, 1] <- timeAxis
        colnames(pdf) <- c("year", keycode_i)
        pdf[pdf[, "year"] < yr, keycode_i] <- 0
        pdf[pdf[, "year"] >= yr, keycode_i] <- NA
        pdf_matrix <- merge(pdf_matrix, pdf, by = "year", all = TRUE)
      
      } else {
        #  apply sapwood_PDF to each individual series
        pdf <- sapwood_PDF(swr = swr_i, last = yr, hdi = FALSE, model = model)
        pdf <- pdf[c(1, 3)] # remove column "swr" from output sapwood_PDF !!! 
        colnames(pdf) <- c("year", keycode_i)
        pdf_matrix <- merge(pdf_matrix, pdf, by = "year", all = TRUE)
        # fill matrix with 0's when NA
        pdf_matrix[is.na(pdf_matrix[, keycode_i]), keycode_i] <- 0
      }
    }
    
    # result of prod(c(NA,NA), na.rm TRUE) == 1
    # first remove rows with all NA (except "year")
    pdf_matrix <- pdf_matrix[rowSums(is.na(pdf_matrix)) != ncol(pdf_matrix)-1, ]
    
    # combine probabilities into one probability density function
    if (n == 1){
      
      pdf_matrix$COMB <- pdf_matrix[, 2]
      
    } else {
      
      pdf_matrix$COMB <- apply(pdf_matrix[, -1], 1, FUN=function(x) prod(x, na.rm = TRUE))
      
      if (any(pdf_matrix[, 2:length(keycodes) +1] == 1, na.rm = TRUE)) {
        # when multiple exact felling dates are listed that do no correspond --> COMB = 0 and after scaling NaN (division by 0)
        # check rowwise if there is any p-value ==1 and replace COMB at that position with 1
        pdf_matrix[apply(pdf_matrix[,2:length(keycodes)+1] == 1, 1, FUN=function(x) any(x, na.rm = TRUE)), "COMB"] <- 1
        
      } else if (sum(pdf_matrix$COMB, na.rm = TRUE) > 0) {
        # avoid division by 0
        pdf_matrix$COMB <- pdf_matrix$COMB/sum(pdf_matrix$COMB, na.rm = TRUE)

      }
    }
    
    if (hdi == FALSE){
      
      return(pdf_matrix)
    
    } else if (hdi == TRUE & sum(pdf_matrix$COMB, na.rm = T) == 0) {
      
      message <- paste0("unable to combine these ", n, " series")
      
      hdi <- c(NA, NA)
      attributes(hdi)$credMass <- credMass
      attributes(hdi)$sapwood_model <- model
      
      summary <- cbind(keycodes, endDate, swr, cambium, NA)
      colnames(summary) <- c("series", "endDate", "swr", "waneyEdge", "A_i")
      
      model_summary <- list(rawData = pdf_matrix,
                            A_c = NA,
                            A_comb = NA,
                            hdi_model = hdi,
                            summary = summary,
                            message = message)
      return(model_summary)
      
  
    } else if (hdi == TRUE & (sum(pdf_matrix[,"COMB"], na.rm =TRUE) >= 2)) {
      # case with multiple and not corresponding felling dates      
      
      fds <- pdf_matrix[pdf_matrix[,  "COMB"] == 1, "year"]
      fds <- paste0(fds, collapse=", ")

      message <- paste0("multiple felling dates: ", fds)
      
      hdi <- c(NA, NA)
      attributes(hdi)$credMass <- credMass
      attributes(hdi)$sapwood_model <- model
      
      summary <- cbind(keycodes, endDate, swr, cambium, NA)
      colnames(summary) <- c("series", "endDate", "swr", "waneyEdge", "A_i")
      
      model_summary <- list(rawData = pdf_matrix,
                            A_c = NA,
                            A_comb = NA,
                            hdi_model = hdi,
                            summary = summary,
                            message = message)
      return(model_summary)

    } else {
      
      require(HDInterval)
      sample <- sample(pdf_matrix$year, size = 100000, replace = TRUE, prob = pdf_matrix$COMB)
      sample <- as.data.frame(sample)
      hdi <- HDInterval::hdi(sample$sample, credMass = credMass, allowSplit = TRUE)
      attributes(hdi)$sapwood_model <- model
          if (hdi[1] == hdi[2]){
            message <- paste("exact felling date: ", hdi[2])
          } else {
            message <- paste("felling date range: ", hdi[1], " - ", hdi[2])
          }
      
      A <- 1
      A_i <- matrix(nrow = length(keycodes), ncol = 1)
      dimnames(A_i) <- list(keycodes, "A_i")
      
      for (i in 1:length(keycodes)){

        if (is.na(swr[i]) & cambium[i] == FALSE){
        A_i[i, 1] <- NA  
          
        } else {
          
        temp  <- apply(pdf_matrix[, c(1+i, length(keycodes)+2)], 1, FUN = function(x) prod(x, na.rm = TRUE))
        temp <- sum(temp, na.rm = TRUE)
        temp2 <- apply(pdf_matrix[, c(1+i, 1+i)], 1, FUN = function(x) prod(x, na.rm = TRUE))
        temp2 <- sum(temp2, na.rm = TRUE)
        A_i[i, 1] <- round(temp/temp2*100, 1)
        A <-  A_i[i, 1]/100 * A
        }
      }
      
      summary <- cbind(keycodes, endDate, swr, cambium, round(A_i, 1))
      colnames(summary) <- c("series", "endDate", "swr", "waneyEdge", "A_i")
      
      A_comb <- round(100 * A^(1/sqrt(length(keycodes))), 1)
      names(A_comb) <- "A_comb"
      
      A_c <- 60
      
      A_n <- round(100 * 1/(sqrt(2*length(keycodes))), 1)
      names(A_c) <- "Ac: critical threshold (%)"
      
      # the logarithmic average of the individual agreement indices 
      A_nc <- round(100 * (1/(sqrt(2*length(keycodes))))^(1/sqrt(length(keycodes))), 1)
      
      model_summary <- list(rawData = pdf_matrix,
                            A_c = A_c,
                            A_comb = A_comb,
                            hdi_model = hdi,
                            summary = summary,
                            message = message)
      return(model_summary)
      
    }
  }
}