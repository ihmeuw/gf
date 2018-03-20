# ----------------------------------------------
  # Audrey Batzel
  #
  # 3/16/18
  # COD PNLP data for 2014-2016; descriptive analysis
# ----------------------------------------------
  
  
# --------------------
  # Set up R / install packages
  rm(list=ls())
  library(data.table)
  library(reshape2)
  library(stringr)
  library(RColorBrewer)
  library(ggplot2)
  library(lubridate)
  library(readxl)
  library(stats)
  library(rlang)
  library(zoo)
  library(tidyr)
  library(dplyr)
# --------------------
  
  
# ----------------------------------------------
  # Overview - Files and Directories
  
  # data directory
    dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data"
 
  # input file:
  # J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/COD_PNLP_Data_Indicators_Long
    # csv files were produced by prep_COD_Malaria_data_function.R
    dt <- fread(paste0(dir,"/","COD_PNLP_Data_Indicators_Long",".csv")) 
    
  # output files:
    # exports all graphs to to a pdf document here:
    # J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series Graphs.pdf
# ----------------------------------------------

    
# ----------------------------------------------        
  # function that takes health zone and indicator as arguments
  # to map each indicator for each health zone over time
    
    # add indicators as facets
 
  makeGraph <- function(hz){
    g <- ggplot(dt[health_zone==hz & subpopulation != "pregnantWomen"], aes(date, newCasesMalariaMild, color = subpopulation, shape=formula_used, ymin=0))
    
    g + geom_point() + geom_line() + theme_bw() + ggtitle(hz) + scale_shape_manual(values=c(1,16))
  }
# ----------------------------------------------  
    
    
# ----------------------------------------------      
  # make a vector of all health zones to loop through 
  hz_vector <- dt[["health_zone"]]

    # remove duplicates:
    hz_vector <- unique(hz_vector)
  
  # loop through vector of health zones to make a graph for each
  pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series Graphs.pdf", height=6, width=9)
  for (h in hz_vector) { 
    print(makeGraph(h))
  }
  dev.off()
# ---------------------------------------------- 