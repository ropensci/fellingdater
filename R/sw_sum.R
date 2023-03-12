#' sw_sum
#'
#' @param x data.frame containing columns c("series", "swr", "waneyedge" , "last"), Column "sw_model" is optional. Could be the output of read_fh(x, header = TRUE)
#' @param scale_p logical, TRUE or FALSE
#' @param series name of the column containing the id's
#' @param last name of the column containing the calendar year assigned to the last measured ring. Should be numeric
#' @param swr name of the column containing the number of sapwood rings. Should be numeric
#' @param waneyedge ame of the column indicating the presence (TRUE)/absence (FALSE) of waney edge. Should be logical
#' @param sw_model apwood data set to be used. Should be one of sw_data_overview()
#' @param credMass scalar [0, 1], specifying the mass within the credible interval (default = .954)
#' @param plot logical, indicates whether a plot of the summed probability  distribution (SPD) should be returned.
#'
#' @description Computes the summed probability density (SPD) for a set of felling date ranges.
#'
#' @return a data.frame
#' @export
#'
sw_sum <- function (x,
                series = "series",
                last = "last",
                n_sapwood = "n_sapwood",
                waneyedge = "waneyedge",
                sw_data = "Hollstein_1980",
                densfun = "lognormal",
                credMass = 0.954,
                plot = FALSE,
                scale_p = FALSE){

        df <- as.data.frame(x)

        cambium <- df[[waneyedge]]
        if (!is.logical(cambium)) {
                warning("Column 'waneyedge' in data.frame should be a logical vector (TRUE/FALSE), indicating the presence of waney edge.\n",
                        "  --> Converted to TRUE/FALSE based on presence of string 'wK'.")
                cambium <- ifelse(grepl("wk", cambium, ignore.case = TRUE), TRUE, FALSE)
        }

        swr <- df[, n_sapwood]
        if (is.character(swr)) {
                stop("--> 'n_sapwood' must be a numeric vector")
        }

        if (any(is.na(swr))) {
                which.nna <- which(is.na(swr) & !cambium)
                df <- df[-which.nna, ]
                swr <- df[, n_sapwood]

                warning(paste0(" --> ", length(which.nna), " Series without sapwood rings or waney edge detected.
                        Those series are removed from the data set")
                )
        }
        cambium <- df[[waneyedge]]

        keycodes <- df[[series]]
        keycodes <- as.character(keycodes)
        if (any(is.na(keycodes))) {
                stop("--> some 'series' have no id")
        }

        endDate <- df[, last]
        if (!is.numeric(endDate)) {
                stop(" --> 'last' must be a numeric vector")
        }

        # sw_model fixed for all series
        if (sw_data %in% sw_data_overview()) {
                sw_data <- rep(sw_data, nrow(df))
        }
        # sw_model might differ between series and is provided in a separate column
        else if (sw_model %in% colnames(df)) {
                sw_data <- df[[sw_data]]
        } else {
                sw_data <- rep("Hollstein_1980", nrow(df))
                warning(" --> Not clear what sapwood model to use. \n
                         'Hollstein_1980' sawpood data set as default for all series.")}

        if (is.na(credMass) || credMass <= 0 || credMass >= 1)
                stop(" --> credMass must be between 0 and 1")


        wk.true <- which(cambium)
        wk.true <- keycodes[wk.true]

        timeRange <- range(df[, last])
        timeAxis <- seq(timeRange[1], timeRange[2] + 100, by = 1)

     pdf_matrix <- matrix(nrow = length(timeAxis), ncol = 1)


     pdf_matrix[, 1] <- timeAxis
     colnames(pdf_matrix) <- "year"

     for (i in 1:length(keycodes)){
          keycode_i <- keycodes[i]
          swr_i <- swr[i]
          yr <- endDate[i]
          cambium_i <- cambium[i]
          sw_data_i <- sw_data[i]

          if (cambium_i == TRUE){
               fellingDate <- df[i, last]
               pdf <- matrix(NA, nrow = 1, ncol = 2)
               colnames(pdf) <- c("year", keycode_i)
               pdf[1,1] <- fellingDate
               pdf[1,2] <- 1
               pdf_matrix <- merge(pdf_matrix, pdf, by = "year", all = TRUE)
          } else {
               pdf <- sw_interval(n_sapwood = swr_i, last = yr, sw_data = sw_data_i, densfun = densfun)
               pdf <- pdf[, -2] # remove column n_sapwood
               colnames(pdf) <- c("year", keycode_i)
               pdf_matrix <- merge(pdf_matrix, pdf, by = "year", all = TRUE)
          }
     }

     # sum probabilities to SPD
     if (length(wk.true > 0)) {
     tmp <- pdf_matrix[, c("year", wk.true)]
     if (dim(tmp)[2] > 2) tmp$SPD_wk <- rowSums(tmp[, -1], na.rm = TRUE)
     if (ncol(tmp) == 2) tmp$SPD_wk <- tmp[, 2]
     if (ncol(tmp) == 1) tmp$SPD_wk <- NA
     } else { pdf_matrix$SPD_wk <- NA}

     tmp <- tmp[, c("year", "SPD_wk")]
     tmp2 <- pdf_matrix[, setdiff(names(pdf_matrix), wk.true)]

     if (dim(tmp2)[2] > 2) pdf_matrix$SPD <- rowSums(tmp2[, -1], na.rm = TRUE)
     if (dim(tmp2)[2] == 2) pdf_matrix$SPD <- tmp2[, 2]
     if (dim(tmp2)[2] == 1) pdf_matrix$SPD <- NA

     pdf_matrix <- merge(pdf_matrix, tmp, by = "year", all = TRUE)

     # scale SPD to 1
     if (scale_p == TRUE){

          pdf_matrix$SPD <- pdf_matrix$SPD/sum(pdf_matrix$SPD, na.rm = TRUE)

     }

     if(plot){
             sw_sum_plot(pdf_matrix)
     }
     else {return(pdf_matrix)
     }
}
