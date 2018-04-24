# ----------------------------------------------
  # Audrey Batzel
  #
  # 3/16/18
  # COD PNLP data for 2014-2016; data quality analysis
  # outlier analysis
  # internal consistency checks
  # descriptive analysis of missing data
# ----------------------------------------------


# --------------------
# Set up R / install packages
  rm(list=ls())
  library(data.table)
  library(reshape2)
  library(stringr)
  library(RColorBrewer)
  library(ggplot2)
  library(GGally)
  library(lubridate)
  library(readxl)
  library(stats)
  library(rlang)
  library(zoo)
  library(tidyr)
  library(gridExtra)
# --------------------


# ----------------------------------------------
  # Overview - Files and Directories
  
  # data directory
    dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data"
  
  # input file:
    # J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/Full data for MI
      fullData <- fread(paste0(dir, "/", "Full Data for MI.csv"))
    # upload excel doc of outliers as a data table to merge with full data set
      outliers <- as.data.table(read_excel(paste0(dir, "/Outliers.xlsx")))
      outliers[, date := as.Date(date)]
  
  # output file: 
    # J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/fullData_forMI_outliers_removed
      
  # Set up:
    fullData[, date := as.Date(date)]
# ----------------------------------------------
    
    
# ----------------------------------------------
  # remove outliers from the data set:
    
    fullDataMelt <- melt(fullData, id.vars= c("V1", "date", "province", "dps", "health_zone"),
                         measure.vars = c(),
                         variable.name = "indicator", 
                         value.name="value")
    
    fullData <- merge(fullDataMelt, outliers, by= c("health_zone", "date", "indicator"), all=T)
    
    
    fullData <- fullData[ is.na(outlier), ]
    
# ----------------------------------------------
    
    
# ----------------------------------------------
  # export data with outliers removed to be used for multiple imputation
    
    write.csv(fullData, paste0("J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/fullData_forMI_outliers_removed.csv"))
    
    