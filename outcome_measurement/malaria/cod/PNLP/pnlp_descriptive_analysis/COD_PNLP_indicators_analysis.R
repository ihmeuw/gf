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

  # output files:
    # exports all exports all health zone graphs to to a pdf document here:
    # J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series Graphs all Indicators.pdf
    
    # exports aggregate graphs to a pdf document here: 
    # J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series Graphs Aggregate Indicators Data.pdf
# ----------------------------------------------


# ----------------------------------------------    
# Set up:
  # convert date column to Date class so x-axis be uniform & chronological
    dt[, date := as.Date(date)]
    dt_wide[, date := as.Date(date)]
    dt[, value := as.numeric(value)]
  
    indicator_names <- c(
      `newCasesMalariaMild` = "Confirmed Cases of Uncomplicated Malaria",
      `newCasesMalariaSevere` = "Cases of Severe Malaria",
      `mildMalariaTreated` = "Cases of Uncomplicated Malaria Treated",
      `severeMalariaTreated` = "Cases of Severe Malaria Treated",
      `malariaDeaths` = "Number of Deaths from Malaria",
      `totalCasesAllDiseases` = "New Cases of All Diseases",
      `suspectedMalaria` = "Suspected Cases of Malaria",
      `totalHospAllDiseases` = "Hospitalized Cases All Diseases", 
      `totalDeathsAllDiseases` = "Number of Deaths from All Diseases",
      `presumedMalaria` = "Presumed Cases of Malaria"
    )
# ----------------------------------------------
    
    
# ----------------------------------------------    
# ----------------------------------------------        
# Graph each INDICATOR for each health zone over time
# ----------------------------------------------    
  # function to develop graphs for each health zone  
    makeGraph <- function(hz){
      g <- ggplot(dt[health_zone==hz & subpopulation != "pregnantWomen"], aes(date, value, color = subpopulation, ymin=0))
    
      #g + geom_point(aes(shape=formula_used)) + geom_line() + theme_bw() + ggtitle(hz) + scale_shape_manual(values=c(16,1)) + facet_wrap("indicator", scales="free_y", labeller = as_labeller(indicator_names))
      g + geom_line() + theme_bw() + ggtitle(hz) + scale_shape_manual(values=c(16,1)) + facet_wrap("indicator", scales="free_y", labeller = as_labeller(indicator_names))
      
      }
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
    

# ----------------------------------------------     
# ---------------------------------------------- 
# Graph the aggregate data for each intervention by DPS over time
# ---------------------------------------------- 
    # vector of all ndicators:
      indicatorInput <- dt[["indicator"]]
      indicatorInput <- unique(indicatorInput)
    
    # need SD cols in wide format
    # test_table <- dt_wide[, .(tot_mal_deaths=sum(malariaDeaths, na.rm=TRUE), tot_mal_deaths=sum(malariaDeaths, na.rm=TRUE)), by =.(date, dps)]
    
    # sum everything at the province level
      #test_table <- dt[, lapply(.SD, sum), by=c('date', 'dps', 'indicator', 'subpopulation'), .SDcols='value']
      test_table2 <- dt[, .(aggValue = sum(value, na.rm=TRUE)), by=c('date', 'dps', 'indicator', 'subpopulation')]
      
    # 
    # for(i in indicatorInput) {
    #   aggGraph <- ggplot(test_table2, aes_string(x='date', y= aggValue, color = 'dps')) + geom_point() + geom_line() 
    #   aggGraph <- aggGraph + theme_bw() + ggtitle("Aggregate Data by DPS") + labs(x= "Date", y="", color= "DPS")
    # }
    # 
    # aggTable <- dt[, .(value = sum(value, na.rm=TRUE)), by =.(date, province, indicator, subpopulation)] 
    # 
    
    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Aggregate Indicators Data.pdf", height=6, width=9)
    for ( i in indicatorInput) {   
      aggGraphTitle <- indicator_names[i]
      aggGraph <- ggplot(test_table2[indicator == i], aes(x=date, y=aggValue, color = subpopulation)) + geom_point() + geom_line() 
      aggGraph <- aggGraph + theme_bw() + ggtitle(paste0("Aggregate Data by Provincial Health Division (DPS): ", aggGraphTitle)) + labs(x= "Date", y="Value", color= "Subpopulation") + facet_wrap(~ dps, scales="free_y") 
      aggGraph <- aggGraph + scale_color_manual(labels = c("Ages 5 and Older", "Pregnant Women", "Ages 4 and Under"), values = c("steelblue4", "palegreen4", "steelblue1"))
      print(aggGraph)
      }
    dev.off()
# ----------------------------------------------     
# ---------------------------------------------- 
    

# ---------------------------------------------- 
# ---------------------------------------------- 
# Graph the aggregate data for each intervention at national level over time
# ---------------------------------------------- 
# sum everything at the national level 
  nationalSum <- dt[, .(aggValue = sum(value, na.rm=TRUE)), by=c('date', 'intervention', 'intervention_spec')]
  
  # ----------------------------------------------  
  makeGraph <- function(i) {
    #aggGraphTitle <- intervention_names[i]
    if (!all(is.na(dt[intervention==interventions[i]]$intervention_spec))) {
      m <- ggplot(data= nationalSum[intervention==interventions[i]], aes(date, aggValue, color = intervention_spec, ymin=0)) + 
        geom_point() + geom_line() + theme_bw() + ggtitle(paste0("DRC: ", intervention_names[i] ))
    }
    if (all(is.na(dt[intervention==interventions[i]]$intervention_spec))) { 
      m <- ggplot(data= nationalSum[intervention==interventions[i]], aes(date, aggValue, ymin=0)) + 
        geom_point() + geom_line() + theme_bw() + ggtitle(paste0("DRC: ", intervention_names[i] ))
    }
    return(m)
  }
  # ----------------------------------------------      
  # Export Graphs to a PDF  
  pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/National-Level Indicators.pdf", height=6, width=9) 
  # Loop through each health zone and use makeGraph to create the graph within a set of plots
  plots1 <- lapply(c(1, 2, 4, 10), function(i) makeGraph(i))
  plots2 <- lapply(c(3, 5, 6, 9), function(i) makeGraph(i))
  plots3 <- lapply(c(7, 8), function(i) makeGraph(i))
  
  # use do.call() to arrange the set of plots on the same page, like facets, but with their own legends
  do.call(grid.arrange, (plots1))
  do.call(grid.arrange, (plots2))
  do.call(grid.arrange, (plots3))
  dev.off()
# ---------------------------------------------- 
# ---------------------------------------------- 