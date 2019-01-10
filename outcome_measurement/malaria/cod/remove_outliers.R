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
    dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/"
  
  # input file:
    # wide data
      fullData <- fread(paste0(dir, "PNLP_2010to2017_fullPrepped.csv"))
    # upload excel doc of outliers as a data table to merge with full data set
      outliers <- as.data.table(read_excel(paste0(dir, "Outliers to Remove.xlsx")))
      outliers[, date := as.Date(date)]
  
  # output file: 
    # "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/fullData_outliers_removed.csv"
      
  # set up:
    fullData[, date := as.Date(date)]
    fullData[, id:=NULL]
    setnames(fullData, "V1", "id")
# ----------------------------------------------
    
    #use dt_wide instead
# ----------------------------------------------
  # remove outliers from the data set:
    all_vars <- c(colnames(fullData))
    
    id_vars <- c("id", "province", "dps", "health_zone", "donor", "operational_support_partner", "population", "quarter", "month",
                 "year", "stringdate", "date") 
    
    
    num_vars <- all_vars[!all_vars %in% id_vars]
    
    fullDataLong <- melt(fullData, id.vars= id_vars,
                         measure.vars = num_vars,
                         variable.name = "indicator", 
                         value.name="value")
    
    fullDataOutliers <- merge(fullDataLong, outliers, by= c("health_zone", "date", "indicator", "value"), all.x=T)
    
    
    fullData <- fullDataOutliers[ is.na(outlier), ]
    
    fullData <- fullData[, c(1:14)]
    setnames(fullData, "province.x", "province")
    fullData <- dcast.data.table(fullData, id + province + dps + health_zone + donor + operational_support_partner + population + quarter + month + year + stringdate + date ~ indicator) 
# ----------------------------------------------
    
    
# ----------------------------------------------
  # export data with outliers removed to be used for multiple imputation
    
    write.csv(fullData, paste0("J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/fullData_outliers_removed.csv"))
    
    