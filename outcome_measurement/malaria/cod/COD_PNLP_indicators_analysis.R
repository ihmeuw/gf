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
  library(gridExtra)
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
    dt2 <- fread(paste0(dir,"/", "COD_PNLP_Data_Interventions_Long",".csv"))
    
  # output files:
    # exports all graphs to to a pdf document here:
    # J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series Graphs.pdf
# ----------------------------------------------

    
# ----------------------------------------------        
# Graph each INDICATOR for each health zone over time
    
  # convert date column to Date class so x-axis be uniform & chronological
    dt[, date := as.Date(date)]
    dt_wide[, date := as.Date(date)]
    dt[, value := as.numeric(value)]
    dt_wide[, value := as.numeric(value)]
  
    indicator_names <- c(
      `newCasesMalariaMild` = "Incidence of Mild Malaria",
      `newCasesMalariaSevere` = "Incidence of Severe Malaria",
      `mildMalariaTreated` = "Cases of Mild Malaria Treated",
      `severeMalariaTreated` = "Cases of Severe Malaria Treated",
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
# Export Graphs to a PDF
    
  # make a vector of all health zones in dt to loop through 
    hz_vector <- dt[["health_zone"]]

    # remove duplicates:
      hz_vector <- unique(hz_vector)
  
  # loop through vector of health zones to make a set of graphs for each and export to 
  # a pdf file
    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series Graphs all indicators.pdf", height=6, width=9)
    for (h in hz_vector) { 
      print(makeGraph(h))
    }
    dev.off()
# ---------------------------------------------- 
    

# ---------------------------------------------- 
  
    
    # vector of all indicators:
    indicatorInput <- dt[["indicator"]]
    indicatorInput <- unique(indicatorInput)
    
    
# Graph the aggregate data for each indicator and subpopulation by province for each month
    test_table <- dt_wide[, .(tot_mal_deaths=sum(malariaDeaths, na.rm=TRUE), tot_mal_deaths=sum(malariaDeaths, na.rm=TRUE)), by =.(date, province)]
    
    # sum everything at the province level
    test_table2 <- dt[, lapply(.SD, sum), by=c('date', 'province', 'subpopulation', 'indicator'), .SDcols='value']
    
    for(i in indicatorInput) {
      aggGraph <- ggplot(test_table, aes_string(x='date', y=i, color = 'province')) + geom_point() + geom_line() 
      aggGraph <- aggGraph + theme_bw() + ggtitle("Aggregate Data by Province") + labs(x= "Date", y="Deaths from Malaria", color= "Provinces")
    }
    
    aggTable <- dt[, .(value = sum(value, na.rm=TRUE)), by =.(date, province, indicator, subpopulation)] 
    
    
    aggGraph <- function(indicatorName){
      
      
        
      aggGraph <- ggplot(test_table, aes(x=date, y=yValue, color = province)) + geom_point() + geom_line() 
      aggGraph <- aggGraph + theme_bw() + ggtitle("Aggregate Data by Province") + labs(x= "Date", y="", color= "Provinces") 
            #aggGraph <- aggGraph + facet_wrap("indicator", scales="free_y", labeller = as_labeller(indicator_names))
    }
    
    for ( i in indicatorInput) { 
      print(aggGraph(i))
    }
# ----------------------------------------------     
