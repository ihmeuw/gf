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
  
# Graph the aggregate data for each indicator and subpopulation by province for each month, each year

  dt <- dt[, province_sum:=sum(value), by=list(province, date, indicator, subpopulation)]
    
    sumProvince <- ggplot(dt, aes(date, province_sum, color = province, ymin=0))
    
    sumProvince + geom_point() + geom_line() + theme_bw() + ggtitle("Aggregate Data") + scale_shape_manual(values=c(16,1)) + facet_wrap("indicator", scales="free_y", labeller = as_labeller(indicator_names))
    
# ----------------------------------------------     
  
  
# ----------------------------------------------        
# Graph each INTERVENTION for each health zone over time
  
  # convert date column to Date class so x-axis be uniform & chronological
  dt2[, date := as.Date(date)]
  dt2[, value := as.numeric(value)]
  
  intervention_names <- c(
    `ArtLum` = "Artéméther - Lumefatrine",
    `SP` = "SP administered during ANC",
    `ASAQ` = "Artesunate Amodiaquine (ACT)",
    `ITN` = "ITNs",
    `ANC` = "Antenatal Care Visits",
    `RDT` = "Rapid Diagnostic Tests",
    `smearTest` = "Smear Tests",
    `VAR` = "Measles Vaccine",
    `healthFacilities` = "Health Facilities Reporting",
    `reports` = "Number of Reports"
  )
  
  treatments <- c("ArtLum", "SP", "ASAQ", "ITN")
  tests <- c("VAR", "ANC", "RDT", "smearTest")
  completenessMeasures <- c("healthFacilities", "reports")
  
# ----------------------------------------------
# ----------------------------------------------  
  # all interventions
  makeGraph2 <- function(hz){
    g2 <- ggplot(dt2[health_zone==hz], aes(date, value, color = intervention_spec, ymin=0))
    
    g2 + geom_point() + geom_line() + theme_bw() + ggtitle(hz) + facet_wrap("intervention", scales="free_y", labeller = as_labeller(intervention_names))
  }

  # "ArtLum", "SP", "ASAQ", "ITN"
  makeGraphTreatments <- function(hz){
    gTreatments <- ggplot(data= subset(dt2[health_zone==hz & (intervention==treatments)]), aes(date, value, color = intervention_spec, ymin=0))
    
    gTreatments + geom_point() + geom_line() + theme_bw() + ggtitle(hz) + facet_wrap("intervention", scales="free_y", labeller = as_labeller(intervention_names))
  }
  
  # "ANC", "RDT", "smearTest", "VAR"
  makeGraphTests <- function(hz){
    gTests <- ggplot(data= subset(dt2[health_zone==hz & (intervention==tests)]), aes(date, value, color = intervention_spec, ymin=0))
    
    gTests + geom_point() + geom_line() + theme_bw() + ggtitle(hz) + facet_wrap("intervention", scales="free_y", labeller = as_labeller(intervention_names))
  }
  
  # "healthFacilities", "reports"
  makeGraphCompleteness <- function(hz){
    gCompleteness <- ggplot(data= subset(dt2[health_zone==hz & (intervention==completenessMeasures)]), aes(date, value, color = intervention_spec, ymin=0))
    
    gCompleteness + geom_point() + geom_line() + theme_bw() + ggtitle(hz) + facet_wrap("intervention", scales="free_y", labeller = as_labeller(intervention_names))
  }
# ----------------------------------------------
# ----------------------------------------------         
  # # antenatal care visits by health zone
  # makeGraphANC <- function(hz){
  #   gANC <- ggplot(data= subset(dt2[health_zone==hz & (intervention==("ANC_1st")| intervention==("ANC_2nd") | intervention==("ANC_3rd") | intervention==("ANC_4th")) ]), aes(date, value, color= intervention, ymin=0))
  #   
  #   gANC + geom_point() + geom_line() + theme_bw() + ggtitle(hz)
  # }
  # 
# ----------------------------------------------
# ----------------------------------------------
  hz <- "Djuma"
  
  
    makeFacet <- function(i) {
      m <- ggplot(data= subset(dt2[health_zone==hz & (intervention==tests[i])]), aes(date, value, color = intervention_spec, ymin=0)) + 
            geom_point() + geom_line() + theme_bw() + ggtitle(tests[i])
      return(m)
    }
    plots <- lapply(1:3, function(i) makeFacet(i))
    
  do.call(grid.arrange, plots)

  
  
  
  
  
# Export Graphs to a PDF
  
  # make a vector of all health zones in dt to loop through 
  hz_vector2 <- dt2[["health_zone"]]
  
  # remove duplicates:
  hz_vector2 <- unique(hz_vector2)

  # loop through vector of health zones to make a set of graphs for each and export to 
  # a pdf file
    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series Graphs Interventions.pdf", height=6, width=9)
    for (h in hz_vector2) { 
      print(makeGraph2(h))
    }
    dev.off()
  
  