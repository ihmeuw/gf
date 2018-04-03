# ----------------------------------------------
  # Audrey Batzel
  #
  # 3/16/18
  # COD PNLP data for 2014-2016; descriptive analysis of interventions data
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
   
    dt <- fread(paste0(dir,"/", "COD_PNLP_Data_Interventions_Long",".csv"))
  
  # output files:
    # exports all health zone graphs to to a pdf document here:
        # J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series for Interventions.pdf
    
    # exports aggregate graphs to a pdf document here: 
        # J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Aggregate Interventions Data.pdf
# ----------------------------------------------


# ----------------------------------------------      
# Set up:
  # convert date column to Date class so x-axis will be uniform & chronological
    dt[, date := as.Date(date)]
    dt[, value := as.numeric(value)]
    
  # # go back to the prep code to make all "value"s numeric and this code will show you where those are 
  #   dt[, numValue := as.numeric(value)]
  #   dt[is.na(numValue) & value!='NA']

  # make a vector of all health zones in dt to loop through 
    hz_vector <- dt[["health_zone"]]
    
  # remove duplicates so only one instance of each health zone remains
    hz_vector <- unique(hz_vector)
    
  # make a vector for intervention names to be used in the graphs
    # go back and edit code to use the first vector here "intervention_names"
    # instead of the "names" vector, to be more sure that it is doing the right one?
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
    
    interventions <- unique(dt$intervention)
    names = c("Antenatal Care Visits", "SP Adminstered at ANC", "Insecticide-Treated Nets", "ASAQ", "Smear Test (Goutte Epaisse)",
              "Rapid Diagnostic Test", "Reports", "Health Facilities Reporting", "Measles Vaccine (VAR)", "Artemether Lumefantrine")

# ---------------------------------------------- 
    
    
# ----------------------------------------------     
# ----------------------------------------------        
# Graph each intervention for each health zone over time
# ----------------------------------------------
# Make a function that creates a ggplot for each indicator for each health zone:  
    
  makeGraph <- function(i, hz) {
    if (!all(is.na(dt[intervention==interventions[i]]$intervention_spec))) {
      m <- ggplot(data= dt[health_zone==hz & intervention==interventions[i]], aes(date, value, color = intervention_spec, ymin=0)) + 
        geom_point() + geom_line() + theme_bw() + ggtitle(paste0(hz, ": ", names[i] ))
    }
    if (all(is.na(dt[intervention==interventions[i]]$intervention_spec))) { 
      m <- ggplot(data= dt[health_zone==hz & intervention==interventions[i]], aes(date, value, ymin=0)) + 
        geom_point() + geom_line() + theme_bw() + ggtitle(paste0(hz, ": ", names[i] ))
    }
    return(m)
  }
# ----------------------------------------------      
# Export Graphs to a PDF  
  pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series for Interventions.pdf", height=6, width=9) 
  # Loop through each health zone and use makeGraph to create the graph within a set of plots
  for (h in hz_vector) { 
    plots1 <- lapply(c(1, 2, 4, 10), function(i, hz) makeGraph(i, h))
    plots2 <- lapply(c(3, 5, 6, 9), function(i, hz) makeGraph(i, h))
    plots3 <- lapply(c(7, 8), function(i, hz) makeGraph(i, h))
    
    # use do.call() to arrange the set of plots on the same page, like facets, but with their own legends
    do.call(grid.arrange, (plots1))
    do.call(grid.arrange, (plots2))
    do.call(grid.arrange, (plots3))
  }  
  dev.off()
# ---------------------------------------------- 
# ----------------------------------------------  
  
  
# ---------------------------------------------- 
# ---------------------------------------------- 
# Graph the aggregate data for each intervention by DPS over time
# ---------------------------------------------- 
  # vector of all indicators:
  interventionInput <- dt[["intervention"]]
  interventionInput <- unique(interventionInput)
  
  interventionInput <- setdiff(interventionInput, "VAR")
 
  # sum everything at the dps level
      #test_table <- dt[, lapply(.SD, sum), by=c('date', 'dps', 'intervention', 'intervention_spec'), .SDcols='value']
  test_table2 <- dt[, .(aggValue = sum(value, na.rm=TRUE)), by=c('date', 'dps', 'intervention', 'intervention_spec')]
  
  makeGraph <- function(i){
    aggGraphTitle <- intervention_names[i]
    aggGraph <- ggplot(test_table2[intervention == i], aes(x=date, y=aggValue, color = intervention_spec)) + geom_point() + geom_line() 
    aggGraph <- aggGraph + theme_bw() + ggtitle(paste0("Aggregate Data by Provincial Health Division (DPS): ", aggGraphTitle)) + labs(x= "Date", y="Value", color= "") + facet_wrap(~ dps, scales="free_y") 
    return(aggGraph)
  }

  # makeGraph <- function(i){
  #   if (!all(is.na(dt[intervention==interventions[i]]$intervention_spec))) {
  #     aggGraphTitle <- intervention_names[i]
  #     aggGraph <- ggplot(test_table2[intervention == i], aes(x=date, y=aggValue, color = intervention_spec)) + geom_point() + geom_line() 
  #     aggGraph <- aggGraph + theme_bw() + ggtitle(paste0("Aggregate Data by Provincial Health Division (DPS): ", aggGraphTitle)) + labs(x= "Date", y="Value", color= "") + facet_wrap(~ dps, scales="free_y") 
  #     return(aggGraph)
  #   }
  #   if (all(is.na(dt[intervention==interventions[i]]$intervention_spec))) {
  #     aggGraphTitle <- intervention_names[i]
  #     aggGraph <- ggplot(test_table2[intervention == i], aes(x=date, y=aggValue)) + geom_point() + geom_line() 
  #     aggGraph <- aggGraph + theme_bw() + ggtitle(paste0("Aggregate Data by Provincial Health Division (DPS): ", aggGraphTitle)) + labs(x= "Date", y="Value", color= "") + facet_wrap(~ dps, scales="free_y") 
  #     return(aggGraph)
  #   }
  # }
  
  pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Aggregate Interventions Data.pdf", height=6, width=9)
  for ( i in interventionInput) {   
    print(i)
    print(makeGraph(i))
  }
  dev.off()
# ----------------------------------------------     
# ----------------------------------------------    
  