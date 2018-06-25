# ----------------------------------------------
  # Audrey Batzel
  #
  # 6/15/18
  # prep of PNLP data for multiple imputation
    setwd('C:/local/gf/')
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
      dir_prepped <-"J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/"
      
    # input files
      fullData <- "fullData_dps_standardized.csv"
      
    # output files
      output_dt <- "PNLP_2010to2017_preppedForMI.csv"
# ----------------------------------------------   


# ----------------------------------------------     
  # read in data
    fullData <- fread( paste0(dir_prepped, fullData) ) 
# ----------------------------------------------     
      
      
# ----------------------------------------------     
  # ADD TO PREP CODE ****************************** change where month/date is duplicated (in original data) ******************************
      fullData[health_zone=='kwamouth' & dps=='mai-ndombe' & date=="2016-01-01" & ANC_1st == "342", date:= "2016-07-01" ]
      fullData[health_zone=='kwamouth' & dps=='mai-ndombe' & date=="2016-02-01" & ANC_1st == "320", date:= "2016-08-01" ]
      fullData[health_zone=='kwamouth' & dps=='mai-ndombe' & date=="2016-03-01" & ANC_1st == "326", date:= "2016-09-01" ]

      fullData[health_zone=='kwamouth' & dps=='mai-ndombe' & date=="2017-01-01" & ANC_1st == "489", date:= "2017-07-01" ]
      fullData[health_zone=='kwamouth' & dps=='mai-ndombe' & date=="2017-02-01" & ANC_1st == "551", date:= "2017-08-01" ]
      fullData[health_zone=='kwamouth' & dps=='mai-ndombe' & date=="2017-03-01" & ANC_1st == "596", date:= "2017-09-01" ]
      
      fullData[health_zone=='mushie' & dps=='mai-ndombe' & date=="2016-01-01" & ANC_1st == "359", date:= "2016-07-01" ]
      fullData[health_zone=='mushie' & dps=='mai-ndombe' & date=="2016-02-01" & ANC_1st == "438", date:= "2016-08-01" ]
      fullData[health_zone=='mushie' & dps=='mai-ndombe' & date=="2016-03-01" & ANC_1st == "415", date:= "2016-09-01" ]
      
      fullData[health_zone=='mushie' & dps=='mai-ndombe' & date=="2017-01-01" & ANC_1st == "370", date:= "2017-07-01" ]
      fullData[health_zone=='mushie' & dps=='mai-ndombe' & date=="2017-02-01" & ANC_1st == "402", date:= "2017-08-01" ]
      fullData[health_zone=='mushie' & dps=='mai-ndombe' & date=="2017-03-01" & ANC_1st == "361", date:= "2017-09-01" ]
      
      fullData[health_zone=='nioki' & dps=='mai-ndombe' & date=="2016-01-01" & ANC_1st == "491", date:= "2016-07-01" ]
      fullData[health_zone=='nioki' & dps=='mai-ndombe' & date=="2016-02-01" & ANC_1st == "446", date:= "2016-08-01" ]
      fullData[health_zone=='nioki' & dps=='mai-ndombe' & date=="2016-03-01" & ANC_1st == "480", date:= "2016-09-01" ]
      
      fullData[health_zone=='nioki' & dps=='mai-ndombe' & date=="2017-01-01" & ANC_1st == "539", date:= "2017-07-01" ]
      fullData[health_zone=='nioki' & dps=='mai-ndombe' & date=="2017-02-01" & ANC_1st == "525", date:= "2017-08-01" ]
      fullData[health_zone=='nioki' & dps=='mai-ndombe' & date=="2017-03-01" & ANC_1st == "617", date:= "2017-09-01" ]
# ----------------------------------------------   

      
# ----------------------------------------------     
  # take a subset of fullData that will be used in MI
    
    all_vars <- c(colnames(fullData))
    
    # remove unneccessary id variables
    id_vars <- c("V1", "province", "dps", "dps_in_original_data", "health_zone", "donor", "operational_support_partner", "population", "quarter", "month",
                 "year", "stringdate", "date", "natl", "natl_name", "province11", "province11_name", "province26", "province26_name", "dps_name_2015", "dps_name_2014", 
                 "dps_name_2013", "dps_name_2012", "dps_name_2010to2011")
    
    measured_vars <- all_vars[!all_vars %in% id_vars]
    id_vars <- c("province11_name", "dps", "health_zone", "date", "year")
    vars_to_keep <- c(id_vars, measured_vars)
    
    ameliaDT <- fullData[, c(vars_to_keep), with=F]
                         
    # remove repetitive or not useful variables 
    vars_to_remove <- c("reports_expected", "reports_received", "ASAQused_total", "peopleTested_5andOlder", "peopleTested_under5", "PMA_ASAQ", "PMA_TPI", "PMA_ITN", "PMA_complete")
    ameliaDT <- ameliaDT[, -c(vars_to_remove), with=F]
    
  # new column to factor in the product of number of health facilities reporting and total number of health facilties - check with David, is this needed?
    ameliaDT$healthFacilitiesProduct <- ameliaDT[, .(healthFacilities_total * healthFacilities_numReported)]
  
  # combine age groups for tests to account for where these are separated out in different years of data- check with David, is this okay? best way to do this?
    ameliaDT[, RDT_completed := ifelse( year <= 2014, RDT_completed, (RDT_completedUnder5 + RDT_completed5andOlder))]
    ameliaDT[, RDT_positive := ifelse( year <= 2014, RDT_positive, (RDT_positiveUnder5 + RDT_positive5andOlder))]
    ameliaDT[, smearTest_completed := ifelse( year <= 2014, smearTest_completed,(smearTest_completedUnder5 + smearTest_completed5andOlder))]
    ameliaDT[, smearTest_positive := ifelse( year <= 2014, smearTest_positive, (smearTest_positiveUnder5 + smearTest_positive5andOlder))]
    
    ameliaDT <- ameliaDT[, -c("year", "smearTest_completedUnder5", "smearTest_completed5andOlder", "smearTest_positiveUnder5", "smearTest_positive5andOlder",
                              "RDT_positive5andOlder", "RDT_positiveUnder5", "RDT_completedUnder5", "RDT_completed5andOlder")]
# ----------------------------------------------     
    
    
# ----------------------------------------------   
  # rectangularize the data so there is an observation for every health zone and date
    hzDPS <- unique(ameliaDT[,c('province11_name', 'dps','health_zone'),with=FALSE])
    dates <- unique(ameliaDT$date)
    
    rect <- hzDPS[rep(1:nrow(hzDPS), length(unique(dates)))]
    rect[, date:=rep(dates, each=nrow(hzDPS))]
    
    dt <- merge(ameliaDT, rect, by=c("date", "province11_name", "health_zone", "dps"), all=TRUE)
    
  # add an id column
    dt[, id:= .I]
# ----------------------------------------------     
    
    
# ----------------------------------------------  
  # export data  
  write.csv(dt, paste0(dir_prepped, output_dt))
# ---------------------------------------------- 