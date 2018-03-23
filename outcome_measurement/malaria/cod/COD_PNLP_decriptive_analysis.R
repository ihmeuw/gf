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
    dt2 <- fread(paste0(dir,"/", "COD_PNLP_Data_Interventions_Long",".csv"))
    
  # output files:
    # exports all graphs to to a pdf document here:
    # J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series Graphs.pdf
# ----------------------------------------------

    
# ----------------------------------------------        
# Graph each INDICATOR for each health zone over time
    
  # convert date column to Date class so x-axis be uniform & chronological
    dt[, date := as.Date(date)]
  
    indicator_names <- c(
      `newCasesMalariaMild` = "Incidence of Mild Malaria",
      `newCasesMalariaSevere` = "Incidence of Severe Malaria",
      `mildMalariaTreated` = "Number of Mild Malaria Cases Treated",
      `severeMalariaTreated` = "Number of Severe Malaria Cases Treated",
      `malariaDeaths` = "Number of Deaths from Malaria"
    )
# ----------------------------------------------
# ----------------------------------------------
  # function to develop graphs for each health zone  
    makeGraph <- function(hz){
      g <- ggplot(dt[health_zone==hz & subpopulation != "pregnantWomen"], aes(date, value, color = subpopulation, ymin=0))
    
      g + geom_point(aes(shape=formula_used)) + geom_line() + theme_bw() + ggtitle(hz) + scale_shape_manual(values=c(16,1)) + facet_wrap("indicator", scales="free_y", labeller = as_labeller(indicator_names))
    }
# ----------------------------------------------
# ---------------------------------------------- 
  # make a vector of all health zones in dt to loop through 
    hz_vector <- dt[["health_zone"]]

    # remove duplicates:
      hz_vector <- unique(hz_vector)
      
# ----------------------------------------------
# ----------------------------------------------  
  # loop through vector of health zones to make a set of graphs for each and export to 
  # a pdf file
    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series Graphs all indicators.pdf", height=6, width=9)
    for (h in hz_vector) { 
      print(makeGraph(h))
    }
    dev.off()
# ---------------------------------------------- 
  
  
  
# ----------------------------------------------        
# Graph each INTERVENTION for each health zone over time
  
  # convert date column to Date class so x-axis be uniform & chronological
  dt2[, date := as.Date(date)]
  dt2[, value := as.numeric(value)]
# ----------------------------------------------
# ----------------------------------------------  
  # all interventions
  makeGraph2 <- function(hz){
    g2 <- ggplot(dt2[health_zone==hz], aes(date, value, ymin=0))
    
    g2 + geom_point() + theme_bw() + ggtitle(hz) + facet_wrap("intervention", scales="free_y")
  }
       
  # antenatal care visits by health zone
  makeGraphANC <- function(hz){
    gANC <- ggplot(data= subset(dt2[health_zone==hz & (intervention==("ANC_1st")| intervention==("ANC_2nd") | intervention==("ANC_3rd") | intervention==("ANC_4th")) ]), aes(date, value, color= intervention, ymin=0))
    
    gANC + geom_point() + geom_line() + theme_bw() + ggtitle(hz)
  }
# ----------------------------------------------
# ----------------------------------------------
  # make a vector of all health zones in dt to loop through 
  hz_vector2 <- dt2[["health_zone"]]
  
  # remove duplicates:
  hz_vector2 <- unique(hz_vector2)
  
# ----------------------------------------------
# ----------------------------------------------  
  # loop through vector of health zones to make a set of graphs for each and export to 
  # a pdf file
    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series Graphs all indicators.pdf", height=6, width=9)
    for (h in hz_vector2) { 
      print(makeGraph2(h))
    }
    dev.off()
  
  