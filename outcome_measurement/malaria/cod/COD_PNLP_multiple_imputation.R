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
    library(stringr)
    library(reshape2)
    library(ggplot2)
    library(lubridate)
    library(readxl)
    library(stats)
    library(Rcpp)
    library(Amelia)
# --------------------  


# ----------------------------------------------
  # Overview - Files and Directories
  
  # data directory
    dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data"
  
  # input file:
  # J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/COD_PNLP_Data_Indicators_Long
  # csv files were produced by prep_COD_Malaria_data_function.R
    dt <- fread(paste0(dir,"/","COD_PNLP_Data_Indicators_Long",".csv")) 
    dt_wide <- fread(paste0(dir,"/","COD_PNLP_Data_Indicators_Wide",".csv")) 
  
  # output files:
    # exports all exports all health zone graphs to to a pdf document here:
    # J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series Graphs all Indicators.pdf
    
    # exports aggregate graphs to a pdf document here: 
    # J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series Graphs Aggregate Indicators Data.pdf
# ----------------------------------------------
    dt_wide[, date := as.Date(date)]
    dt_wide[, population := as.numeric(population)]
    
    amelia(dt_wide, m=5, ts="date", cs= "health_zone", idvars= c("province", "dps"))
     
    