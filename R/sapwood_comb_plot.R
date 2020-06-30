# #### sapwood_comb_plot ----
#
sapwood_comb_plot <- function(x, credMass = 0.954, model = "Hollstein_1980"){
  
  # function to rescale probabilities between [0, 1].
  rescale <- function(x, floor = 0, ceiling = 1) 
    if (max(x, na.rm = T) == 0) {x} 
    else{(x - min(x, na.rm = T))*(ceiling - floor)/(max(x, na.rm = T) - min(x, na.rm = T))}
  
  if (!require(tidyverse)) {install.packages("tiduverse")} else {library(tidyverse)}
  
  # # computes a data frame with the combined probability, based on x = c(keycode, Date_end, SWR, Waneyedge)
  output <- sapwood_combine(x, credMass = credMass, hdi = TRUE, model = model)
  
  n <- nrow(output$summary)
    
  yrs <- output$rawData["year"]
  pdf <- apply(output$rawData[, 2:ncol(output$rawData)], 2, FUN = rescale)
  pdf <- cbind(yrs, pdf)
    
  combo <- pdf %>% select(year, COMB)
    
  range <- range(pdf$year)
  range[2] <- range(x[, 2])[2]
    
  # creates a vector with series that only have a terminus post quem
  tpq <- which(apply(output$rawData, 2, FUN = function(x) sum(x, na.rm = TRUE)) == 0)
  tpq <- names(tpq)
  tpq_min <- sapwood_PDF(0, hdi = TRUE, credMass= credMass, model = model)[1]
    
  hdi <- output$hdi
    
  summary <- 
    output$summary %>%
    as_tibble() %>%
    mutate(Agreement = if_else(as.numeric(A_i) < 60, "poor", "good"))
    
  pdf <- 
    pdf %>%
    pivot_longer(-year, names_to = "series", values_to = "p") %>%
    filter(series != "COMB")
    
  ggplot(pdf, aes(x= year, y = p)) +
      
    # plot the pdf for each series with swr or exact felling date
    { if (length(tpq) < nrow(output$summary))
      geom_area(fill = "grey90", alpha = 0.7) } +
    { if (length(tpq) < nrow(output$summary))
      geom_line(color = "grey40", size = 0.5) } +
      
    # # plot tpq as arrow pointing away from end date
    { if (length(tpq) > 0)
      geom_point(data = pdf %>% filter(series %in% tpq) %>% filter(!is.na(p)) %>% group_by(series) %>% summarize(max = max(year)),
                 aes(x = max + 1, y = .5),
                 size = 2, fill = "darkblue") } +
    { if (length(tpq) > 0)
      geom_segment(data = pdf %>% filter(series %in% tpq) %>% filter(!is.na(p)) %>% group_by(series) %>% summarize(max = max(year)),
                   aes(x = max + 1, xend = max + tpq_min + 1, y = .5, yend = .5, group = series),
                   arrow = arrow(length = unit(0.08, "npc")))
    } +
      
    # # add the combined felling date estimate as background
    { if (!is.na(hdi[2]))
      geom_area(data = combo, aes(x = year, y = COMB), fill = "grey50", alpha = 0.5) } +
    { if (!is.na(hdi[2]))
      geom_line(data = combo, aes(x = year, y = COMB), color = "grey30") } +
      
    # draw a line delimiting the hdi of the combined felling date estimate (credMass)
    {if (!is.na(hdi[2])) 
      geom_segment(aes(x = hdi[1], xend = hdi[2], y = -0.1, yend = -0.1), color = "black", size = 1) } +

    # add summary text
    geom_text(data = summary, aes(x = plyr::round_any(range[2] + 40, 10, ceiling),
                                    y = 0.5, 
                                    label = paste0(
                                      series, " (last: ", endDate, ", swr: ", ifelse(is.na(swr), "- ", swr), ")",
                                      "\n ", ifelse(is.na(A_i), "", paste0("A_i = ", round(as.numeric(A_i), 1), "%"))), hjust = 1)) +
      
    { if (nrow(summary %>% filter(Agreement == "poor")) != 0)
      geom_text(data = summary %>% filter(Agreement == "poor"),
                aes(x = plyr::round_any(range[2] + 40, 10, ceiling),
                    y = 0.2,
                    label = "!! poor agreement (Ac = 60%)",
                    color = "tomato3", hjust = 1)) 
     } +
      
      facet_grid(vars(as.factor(series))) +
      theme_minimal() +
      
      scale_x_continuous(limits = c(plyr::round_any(range[1], 10, floor), plyr::round_any(range[2] + 40, 10, ceiling)),
                         breaks = seq(plyr::round_any(range[1], 10, floor) , plyr::round_any(range[2] + 40, 10, ceiling), 10)) +
      
      theme(axis.text=element_text(size=12),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y = element_blank(),
            panel.grid.minor.y=element_blank(),
            panel.grid.major.y=element_blank(),
            legend.position = "none",
            strip.background = element_blank(),
            strip.text.y = element_blank()
      ) +
    
      labs(title = paste0(output$message),
           subtitle = paste0(if(!is.na(output$A_comb)) paste("A_model = ", output$A_comb, "% (Ac = 60%)"), 
                             if(!is.na(output$A_comb) & output$A_comb <50) "  !! poor agreement > model fails"),
           caption = paste0(100*credMass, "% credible interval (hdi)")) +
  if(output$A_comb < 60 & !is.na(output$A_comb)) {theme(plot.subtitle = element_text(color = "tomato3"))}
  
}