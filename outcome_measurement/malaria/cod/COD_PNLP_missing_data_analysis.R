# ----------------------------------------------
  # Audrey Batzel
  #
  # 3/16/18
  # COD PNLP data for 2014-2016; descriptive analysis of missing data
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
      dt <- fread(paste0(dir,"/","COD_PNLP_Data_Indicators_Long.csv")) 
      dt_wide <- fread(paste0(dir,"/","COD_PNLP_Data_Indicators_Wide.csv")) 
      dt2 <- fread(paste0(dir, "/", "COD_PNLP_Data_Interventions_Long.csv"))
        
    # output files:

    # Set up:
      dt[, date := as.Date(date)]
      dt2[, date := as.Date(date)]
      dt2[, value := as.numeric(value)]
# ----------------------------------------------


# ----------------------------------------------
  # Outlier Analysis - Indicators
  # indicators to loop through and make a histogram for
    indicators <- unique(dt$indicator)
    
    indicator_names <- c(
      `newCasesMalariaMild` = "Incidence of Mild Malaria",
      `newCasesMalariaSevere` = "Incidence of Severe Malaria",
      `mildMalariaTreated` = "Cases of Mild Malaria Treated",
      `severeMalariaTreated` = "Cases of Severe Malaria Treated",
      `malariaDeaths` = "Number of Deaths from Malaria"
    )

    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Outlier Analysis Indicators.pdf", height=6, width=9)   
    for (i in indicators){
    # Subset of data
        dt_byIndicator <- dt[indicator == i,
                             indicator,
                             by= c('date', 'dps', 'health_zone', 'subpopulation', 'value')]
        
    # Make a histogram for each variable
        # qplot(dt_byIndicator$value, geom="histogram")
        
        g <- ggplot(data=dt_byIndicator, aes(dt_byIndicator$value)) + geom_histogram(bins = 50, fill="cornflowerblue") 
        g <- g + ggtitle(indicator_names[i]) + xlab("Value") + ylab("Count") + facet_grid(subpopulation ~ dps, scales="free")
        print(g)
    }
    dev.off()  
# ----------------------------------------------
    
    
# ----------------------------------------------
  # Outlier Analysis - Interventions
  # indicators to loop through and make a histogram for
    interventions <- unique(dt2$intervention)
    
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
    
    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Outlier Analysis Interventions.pdf", height=6, width=9)   
    for (i in interventions){
      # Subset of data
          dt_byIntervention <- dt2[intervention == i,
                               intervention,
                               by= c('date', 'dps', 'health_zone', 'intervention_spec', 'value')]
          
      # Make a histogram for each variable
          g <- ggplot(data=dt_byIntervention, aes(dt_byIntervention$value)) + geom_histogram(bins = 50) 
          g <- g + ggtitle(intervention_names[i]) + xlab("Value") + ylab("Count") + facet_grid(intervention_spec ~ dps, scales="free")
          print(g)
    }
    dev.off()  
# ---------------------------------------------- 
    
      
# ----------------------------------------------
  # Subset of health zone and data and indicators
    
    dt_missing <- dt[, 
                     .(missing_value = ifelse(is.na(value), 1, 0)), 
                      by=c('date', 'dps', 'health_zone', 'indicator', 'subpopulation', 'value')]
    
# ----------------------------------------------
      + facet_wrap(~ dps, scales="free_y")

# ----------------------------------------------
  # Make a histogram for counts of missing data by each month (total)
    
    dt_missing_byDate <- dt_missing[,
                                 .(sumMissing = sum(missing_value)),
                                 by=c('date', 'indicator', 'subpopulation')]
    
    g <- ggplot(data=dt_missing_byDate, aes(x = date, y = sumMissing)) + geom_bar(stat="identity")
    g <- g + ggtitle("Missing Data by Date") + xlab("Date (Month and Year)") + ylab("Count of Missing Values")
    print(g)
    
    g_reorder <- ggplot(dt_missing_byDate, aes(x = reorder(date, sumMissing), y = sumMissing)) + geom_bar(stat="identity")
    g_reorder <- g_reorder + ggtitle("Missing Data by Date") + xlab("Date (Month and Year)") + ylab("Count of Missing Values")
    print(g_reorder)
    
  # Make a histogram for counts of missing data by health zone (total)
    
    dt_missing_byHZ <- dt_missing[,
                                 .(sumMissing = sum(missing_value)),
                                 by=c('health_zone')]
    
    
    g <- ggplot(data=dt_missing_byHZ, aes(x = health_zone, y = sumMissing)) + geom_bar(stat="identity")
    g <- g + ggtitle("Missing Data by Health Zone") + xlab("Health Zone)") + ylab("Count of Missing Values")
    print(g)
    
    g_reorder <- ggplot(dt_missing_byHZ, aes(x = reorder(health_zone, sumMissing), y = sumMissing)) + geom_bar(stat="identity")
    g_reorder <- g_reorder + ggtitle("Missing Data by Health Zone") + xlab("Health Zone") + ylab("Count of Missing Values")
    print(g_reorder)
    
# ----------------------------------------------


# ----------------------------------------------
  # Make a histogram for counts of missing data by each month by indicator
    dt_percentMissing <- dt_missing[, 
                                    .(percent_missing = (mean(missing_value)*100)), 
                                    by=c('date', 'indicator', 'subpopulation')]

    g <- ggplot(data=dt_percentMissing, aes(x = date, y = percent_missing, color = subpopulation)) + geom_bar(stat="identity") + facet_grid(indicator ~ subpopulation)
    g <- g + ggtitle("Missing Data by Indicator and Subpopulation") + ylim(0, 100) + labs(x= "Date", y= "Percent Missing Data") 
    print(g)
# ----------------------------------------------
  # Make a histogram for counts of missing data by health zone by indicator  

    dt2 <- dt_missing[indicator == "newCasesMalariaSevere",
                                    .(percent_missing = (mean(missing_value)*100)), 
                                    by=c('date')]
    
    g <- ggplot(data=dt2, aes(x = date, y = percent_missing)) + geom_bar(stat="identity")
    g <- g + ggtitle("Missing Data for New Cases of Severe Malaria") + ylim(0, 100) + labs(x= "Date", y= "Percent Missing Data") 
    print(g)
    
    
    
    