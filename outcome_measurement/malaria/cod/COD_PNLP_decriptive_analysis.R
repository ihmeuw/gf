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
#******ADD TO PREP CODE:
  # translate french to numeric version of month Janvier=1
        # dt[month=='Janvier', month:="01"]
        # grepl() to make sure that any that may have trailing white space are also changed
        dt[grepl("Janvier", month), month:="01"]
        dt[grepl("Février", month), month:="02"]
        dt[grepl("Mars", month), month:="03"]
        dt[grepl("Avril", month), month:="04"]
        dt[grepl("Mai", month), month:="05"]
        dt[grepl("Juin", month), month:="06"]
        dt[grepl("Juillet", month), month:="07"]
        dt[grepl("Août", month), month:="08"]
        dt[grepl("Septembre", month), month:="09"]
        dt[grepl("Octobre", month), month:="10"]
        dt[grepl("Novembre", month), month:="11"]
        dt[grepl("Décembre", month), month:="12"]
      
    # make sure it worked for all instances of month
    
    # make string version of the date
    dt[, stringdate:=paste('01', month, year, sep='/')]
    
    # combine year and month into one variable
    dt[, date:=as.Date(stringdate, "%d/%m/%Y")]

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