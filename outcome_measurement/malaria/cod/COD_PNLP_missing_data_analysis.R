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
      fullData <- fread(paste0(dir, "/", "Full Data for MI.csv"))
        
    # output files:

    # Set up:
      dt[, date := as.Date(date)]
      dt2[, date := as.Date(date)]
      dt2[, value := as.numeric(value)]
# ----------------------------------------------


# ----------------------------------------------
# Use linear regression to identify outliers
idVars = names(fullData)[1:5]
regVars = names(fullData)[!names(fullData) %in% idVars]

i = 1
outcome = regVars[i]
otherVars = regVars[-i]
formula = paste(c(outcome, '~', paste(otherVars, collapse='+')), collapse='')
formula = as.formula(formula)
lmFit = lm()

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
    
  # Make a histogram for each indicator by supopulation and dps
    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Outlier Analysis Indicators.pdf", height=6, width=9)   
    for (i in indicators){
      # Subset of data
          dt_byIndicator <- dt[indicator == i,
                               indicator,
                               by= c('date', 'dps', 'health_zone', 'subpopulation', 'value')]
     
      # for geom_vline() - calculate the diff percentiles for the data
          p95 <- dt_byIndicator[, quantile(value, .95, na.rm=T), by=c('dps', 'subpopulation')]
          p99 <- dt_byIndicator[, quantile(value, .99, na.rm=T), by=c('dps', 'subpopulation')]
          p995 <- dt_byIndicator[, quantile(value, .995, na.rm=T), by=c('dps', 'subpopulation')]
          p999 <- dt_byIndicator[, quantile(value, .999, na.rm=T), by=c('dps', 'subpopulation')]
          # p95 = dt_byIndicator[, quantile(value, .95, na.rm=T),by=c('dps')]
  
      g <- ggplot(dt_byIndicator, aes(value)) + 
          geom_histogram(bins = 50, fill="cornflowerblue") + 
          ggtitle(paste0("Outlier Analysis for ", indicator_names[i])) + xlab("Value") + ylab("Count") +
          geom_vline(data=p95, aes(xintercept=V1, color="green"), linetype="dashed") + 
          geom_vline(data=p99, aes(xintercept=V1, color="yellow"), linetype="dashed") + 
          geom_vline(data=p995, aes(xintercept=V1, color="orange"), linetype="dashed") + 
          geom_vline(data=p999, aes(xintercept=V1, color="red"), linetype="dashed") + 
          scale_color_manual(name = "Percentile", labels = c("95%", "99%", "99.5%", "99.9%"), values = c("green", "yellow", "orange", "red")) +
          facet_grid(subpopulation~dps, scales='free') + 
          xlab("Value") + ylab("Count")
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
  # Make a histogram for each intervention by subpop and dps
    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Outlier Analysis Interventions.pdf", height=6, width=9)   
    for (i in interventions){
      # Subset of data
          dt_byIntervention <- dt2[intervention == i,
                               intervention,
                               by= c('date', 'dps', 'health_zone', 'intervention_spec', 'value')]
          
      # for geom_vline() - calculate the diff percentiles for the data
          p95 <- dt_byIntervention[, quantile(value, .95, na.rm=T), by=c('dps', 'intervention_spec')]
          p99 <- dt_byIntervention[, quantile(value, .99, na.rm=T), by=c('dps', 'intervention_spec')]
          p995 <- dt_byIntervention[, quantile(value, .995, na.rm=T), by=c('dps', 'intervention_spec')]
          p999 <- dt_byIntervention[, quantile(value, .999, na.rm=T), by=c('dps', 'intervention_spec')]
      
      g <- ggplot(dt_byIntervention, aes(value)) + 
        geom_histogram(bins = 75, fill="cornflowerblue") + 
        ggtitle(paste0("Outlier Analysis for ", intervention_names[i])) + xlab("Value") + ylab("Count") +
        geom_vline(data=p95, aes(xintercept=V1, color="blue"), linetype="dashed") + 
        geom_vline(data=p99, aes(xintercept=V1, color="yellow"), linetype="dashed") + 
        geom_vline(data=p995, aes(xintercept=V1, color="orange"), linetype="dashed") + 
        geom_vline(data=p999, aes(xintercept=V1, color="red"), linetype="dashed") + 
        scale_color_manual(name = "Percentile", labels = c("95%", "99%", "99.5%", "99.9%"), values = c("green", "yellow", "orange", "red")) +
        facet_grid(intervention_spec~dps, scales='free') + 
        xlab("Value") + ylab("Count")
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
    
    
    
    