###----------------------------###
#- Title: Function for converting raw PI data to total species cover
#- Author: Carolyn Livensperger (Matt VanScoyoc, Dana Witwicki)
#- Date: September 1, 2021
#- Related Files: summarize-raw-pi.R
#- This script is a standalone function to convert raw point-intercept data into # of hits and # of trials. 

###----------------------------###


#-------- Packages --------------------------
library('tidyverse')
library('lubridate') 

fx_SummarizePI_Total <- function(data) {
  #---Make list of all species in data---
  obs <- data %>%
    select(matches("^top$|^lcs\\d+$|^surface$")) 
  obs <- data.frame(lapply(obs, as.character), stringsAsFactors=FALSE)
  uniobs <- unique(unlist(obs)) 
  sppobs <- sort(uniobs[-which(uniobs %in% c(NA, "U", "CY", "LC", "M", "S", "L", "WD", "SR", "LR", "BR", "SL", "SW", "WA", "NR"))]) 
  
  #---Create blank matrix of unique species at each point---
  pt.names <- paste0(data$visit_year,  "_", data$unit_code, "_", data$plot_id, "_", data$transect, "_", data$point)
  sppmatrix <- matrix(NA, length(pt.names), length(sppobs))
  rownames(sppmatrix) <- pt.names
  colnames(sppmatrix) <- sppobs
  
  #---Fill in matrix of unique species at each point with 1=True and 0=False---
  for (i in 1:length(sppobs)){
    sppmatrix[,i] <- ifelse((data$top == colnames(sppmatrix)[i]) | 
                              (data$surface == colnames(sppmatrix)[i]) | 
                              (data$lcs1 == colnames(sppmatrix)[i]) | 
                              (data$lcs2 == colnames(sppmatrix)[i]) | 
                              (data$lcs3 == colnames(sppmatrix)[i]) | 
                              (data$lcs4 == colnames(sppmatrix)[i]) | 
                              (data$lcs5 == colnames(sppmatrix)[i]) | 
                              (data$lcs6 == colnames(sppmatrix)[i]) | 
                              (data$lcs7 == colnames(sppmatrix)[i]) | 
                              (data$lcs8 == colnames(sppmatrix)[i]) | 
                              (data$lcs9 == colnames(sppmatrix)[i]) | 
                              (data$lcs10 == colnames(sppmatrix)[i]), 1, 0)
  }
  
  #Not sure why For Loop adds 1's but not 0's. The line of code below fixes this.
  sppmatrix[is.na(sppmatrix)] <- 0
  
  #---Make columns for year, plot, transect, and point---
  sppmatrixlong <- data.frame(as.character(rownames(sppmatrix)), sppmatrix)
  colnames(sppmatrixlong)[1]="unique_id"
  sppstart <- sppobs[[1]]
  sppend <- last(sppobs)
  sppmatrixlong <- sppmatrixlong %>%
    separate(unique_id, c('visit_year', 'unit_code', 'plot_id', 'transect', 'point'), sep = '_')
  
  #Sum total possible hits for each transect
  plot_hits <- sppmatrixlong %>%
    group_by(visit_year, unit_code, plot_id, transect) %>%
    summarise(total_hits = n()) %>%
    ungroup()
  
  #Sum hits for each species
  sp_hits <- sppmatrixlong %>% 
    select(-point) %>%
    group_by(visit_year, unit_code, plot_id, transect) %>%
    summarise_all(sum) %>%
    ungroup()
  
  # Join plot hits
  sp_hits <- left_join(sp_hits, plot_hits, by = c("visit_year", "unit_code", "plot_id", "transect"))
return(list(sp_hits, plot_hits))
}
