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
  
  
# ----------------------------------------------        
# Graph each INTERVENTION for each health zone over time
  
  # convert date column to Date class so x-axis be uniform & chronological
  dt2[, date := as.Date(date)]
  dt2[, value := as.numeric(value)]
  
  # go back to the prep code to make all "value" numeric and this shows you where those are 
  dt2[is.na(numValue) & value!='NA']
  
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
  # # all interventions
  # makeGraph2 <- function(hz){
  #   g2 <- ggplot(dt2[health_zone==hz], aes(date, value, color = intervention_spec, ymin=0))
  #   
  #   g2 + geom_point() + geom_line() + theme_bw() + ggtitle(hz) + facet_wrap("intervention", scales="free_y", labeller = as_labeller(intervention_names))
  # }
  # 
  # # "ArtLum", "SP", "ASAQ", "ITN"
  # makeGraphTreatments <- function(hz){
  #   gTreatments <- ggplot(data= subset(dt2[health_zone==hz & (intervention==treatments)]), aes(date, value, color = intervention_spec, ymin=0))
  #   
  #   gTreatments + geom_point() + geom_line() + theme_bw() + ggtitle(hz) + facet_wrap("intervention", scales="free_y", labeller = as_labeller(intervention_names))
  # }
  # 
  # # "ANC", "RDT", "smearTest", "VAR"
  # makeGraphTests <- function(hz){
  #   gTests <- ggplot(data= subset(dt2[health_zone==hz & (intervention==tests)]), aes(date, value, color = intervention_spec, ymin=0))
  #   
  #   gTests + geom_point() + geom_line() + theme_bw() + ggtitle(hz) + facet_wrap("intervention", scales="free_y", labeller = as_labeller(intervention_names))
  # }
  # 
  # # "healthFacilities", "reports"
  # makeGraphCompleteness <- function(hz){
  #   gCompleteness <- ggplot(data= subset(dt2[health_zone==hz & (intervention==completenessMeasures)]), aes(date, value, color = intervention_spec, ymin=0))
  #   
  #   gCompleteness + geom_point() + geom_line() + theme_bw() + ggtitle(hz) + facet_wrap("intervention", scales="free_y", labeller = as_labeller(intervention_names))
  # }
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
  
  interventions <- unique(dt2$intervention)
  names = c("Antenatal Care Visits", "SP Adminstered at ANC", "Insecticide-Treated Nets", "ASAQ", "Smear Test (Goutte Epaisse)",
            "Rapid Diagnostic Test", "Reports", "Health Facilities Reporting", "Measles Vaccine (VAR)", "Artemether Lumefantrine")
  
    makeFacet <- function(i, hz) {
      if (!all(is.na(dt2[intervention==interventions[i]]$intervention_spec))) {
          m <- ggplot(data= dt2[health_zone==hz & intervention==interventions[i]], aes(date, value, color = intervention_spec, ymin=0)) + 
            geom_point() + geom_line() + theme_bw() + ggtitle(paste0(hz, ": ", names[i] ))
      }
      if (all(is.na(dt2[intervention==interventions[i]]$intervention_spec))) { 
          m <- ggplot(data= dt2[health_zone==hz & intervention==interventions[i]], aes(date, value, ymin=0)) + 
          geom_point() + geom_line() + theme_bw() + ggtitle(paste0(hz, ": ", names[i] ))
      }
      return(m)
    }
    
    #plots <- lapply(seq(length(interventions)), function(i) makeFacet(i))
    
    
    for (h in hz_vector) { 
      plots1 <- lapply(c(1, 2, 4, 10), function(i, hz) makeFacet(i, h))
      plots2 <- lapply(c(3, 5, 6, 9), function(i, hz) makeFacet(i, h))
      plots3 <- lapply(c(7, 8), function(i, hz) makeFacet(i, h))
      
      do.call(grid.arrange, (plots1))
      do.call(grid.arrange, (plots2))
      do.call(grid.arrange, (plots3))
    }  
    
  pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series for Interventions.pdf", height=6, width=9) 
    for (h in hz_vector) { 
      plots1 <- lapply(c(1, 2, 4, 10), function(i, hz) makeFacet(i, h))
      plots2 <- lapply(c(3, 5, 6, 9), function(i, hz) makeFacet(i, h))
      plots3 <- lapply(c(7, 8), function(i, hz) makeFacet(i, h))
      
      do.call(grid.arrange, (plots1))
      do.call(grid.arrange, (plots2))
      do.call(grid.arrange, (plots3))
    }  
  dev.off()
  
  
  
  
  
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
  
  