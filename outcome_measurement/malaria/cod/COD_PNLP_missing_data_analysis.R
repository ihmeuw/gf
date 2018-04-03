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
      dt <- fread(paste0(dir,"/","COD_PNLP_Data_Indicators_Long",".csv")) 
      dt_wide <- fread(paste0(dir,"/","COD_PNLP_Data_Indicators_Wide",".csv")) 
    
    # output files:

    # Set up:
      dt[, date := as.Date(date)]
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