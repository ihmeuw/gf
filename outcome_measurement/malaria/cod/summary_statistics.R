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
  library (dplyr)
# --------------------  


# ----------------------------------------------
  # Overview - Files and Directories
  
  # data directory
    dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/"
  
  # import graphDataComplete
    dt <- as.data.table(read.csv(paste0(dir, "Imputed Data.csv")))

# ----------------------------------------------
    
    
# ----------------------------------------------
  # aggregate every HZ-indicator to the year level and see what the percent change from 2015 to 2016 
  # and 2016 to 2017 is and whether there are any that stand out as very different.
    
    dt$year <- year(dt$date)
    dt$month <- month(dt$date)

    aggByYear <- dt[, .(yearValue = sum(mean)), by=c( "year", "health_zone", "dps", "province", "indicator", "subpopulation")]
    
    aggByYear$indicator = paste(aggByYear$indicator, aggByYear$subpopulation, sep="_")
    
    indicators <- c("newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen",
                    "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen",
                    "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen",
                    "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen")

    interventions <- c("ANC_1st", "ANC_2nd", "ANC_3rd", "ANC_4th", "SP_1st", "SP_2nd","SP_3rd", "ITN_received", "ITN_distAtANC",
                       "ITN_distAtPreschool", "VAR", "ASAQ_2to11mos", "ASAQ_1to5yrs", "ASAQ_6to13yrs", "ASAQ_14yrsAndOlder", "ArtLum_received", "ArtLum_used",
                       "smearTest_completed", "smearTest_positive", "RDT_completed", "RDT_positive", "healthFacilities_total", "healthFacilitiesProduct")
    
    measuredVars <- c(indicators, interventions)
    
    dtWide <- dcast(aggByYear, year + province + dps + health_zone ~ indicator, value.var="yearValue")
    
        
  rateOfChange <- aggByYear %>%
        group_by(indicator) %>%
        arrange(health_zone, year, indicator) %>%
        mutate(rate = 100 * (yearValue - lag(yearValue))/lag(yearValue)) %>%
        ungroup()
      
  rateOfChangeWide <- dcast(rateOfChange, year + province + dps + health_zone ~ indicator, value.var="rate")
  
  
  highValues <- rateOfChange[rate>95, c("health_zone", "year", "rate")]


  # data table sort, with the last variable the one you want to measure rate of change across
  # shift() function
  
  
  id_vars <- colnames(aggByYear)[!colnames(aggByYear) %in% c("year", "yearValue")]
  
  rateOfChange <- aggByYear[order(eval(c(id_vars, "year")))]
  
  []
  