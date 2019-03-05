# ----------------------------------------------
  # Audrey Batzel
  #
  # 6/15/18
  # prep of PNLP data for multiple imputation
    setwd('C:/local/gf/')
  # Cleaned up/edited/redone - 02/19
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
# ---------------------------------------------- 
    # data directory
      dir_prepped <-"J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/"
      
    # input files
      fullData <- "fullData_dps_standardized.csv"
      
    # output files
      output_dt <- "PNLP_2010to2017_preppedForMI_updated_1_22_19.csv"
      
    # functions:
      getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
# ----------------------------------------------   


# ----------------------------------------------     
  # read in data
# ---------------------------------------------- 
    fullData <- fread( paste0(dir_prepped, fullData) ) 
# ----------------------------------------------     
      
      
# ----------------------------------------------     
  # ADD TO PREP CODE ****************************** 
# ----------------------------------------------  
    # change where month/date is duplicated (in original data) ******************************
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
      
  # Where DPS = 0, those health zones (bambu and manguredj) weren't in the most recent year of data (2017) but in retrospect it seems like
  # they should be other health zones/dps - change so they match up
      
      # bambu-nord kivu is probably the same as bambo-nord kivu 
      fullData[ dps == "0" & health_zone == "bambu", dps := "nord kivu"]
      fullData[ dps == "nord kivu" & health_zone == "bambu", health_zone := "bambo"]
      
      # manguredj and mangupa both probably the same ('manguredjipa')
      fullData[ dps == "0" & health_zone == "manguredj", dps := "nord kivu"]
      fullData[ dps == "nord kivu" & health_zone == "manguredj", health_zone := "mangupa"]
# ----------------------------------------------   

      
# ----------------------------------------------     
# take a subset of fullData that will be used in MI
# ---------------------------------------------- 
    all_vars <- c(colnames(fullData))
      
    # remove unneccessary id variables
    id_vars <- c("V1", "province", "dps", "dps_in_original_data", "health_zone", "donor", "operational_support_partner", "population", "quarter", "month",
                 "year", "stringdate", "date", "natl", "natl_name", "province11", "province11_name", "province26", "province26_name", "dps_name_2015", "dps_name_2014", 
                 "dps_name_2013", "dps_name_2012", "dps_name_2010to2011")
    measured_vars <- all_vars[!all_vars %in% id_vars]
    
    id_vars <- c("dps", "health_zone", "date", "donor", "operational_support_partner", "population")
    vars_to_keep <- c(id_vars, measured_vars)
    
    ameliaDT <- fullData[, vars_to_keep, with=FALSE]
                         
    # remove internal totals and/or not useful variables 
    remove_vars <- c("reports_expected", "reports_received", "ASAQused_total", "peopleTested_5andOlder", "peopleTested_under5", "PMA_ASAQ", "PMA_TPI", "PMA_ITN", "PMA_complete")
    ameliaDT <- ameliaDT[, -remove_vars, with=FALSE]
# ----------------------------------------------   


# ---------------------------------------------- 
# Deterministically impute total health facilities and convert the number reporting to a proportion over the total, then drop the original variable for number reporting and impute
    # the proportion so that we can back calculate the number reporting and it will always be less than the total number of facilities.
# ----------------------------------------------   
ameliaDT[, year:= year(date)]
# keep track of what the original values are before we change them:
  ameliaDT[, healthFacilities_total_orig := healthFacilities_total]
  ameliaDT[, healthFacilities_numReported_orig := healthFacilities_numReported]

  ameliaDT$healthFacilities_total <- as.numeric(ameliaDT$healthFacilities_total)
  # NOTE: commented out 01/19 because I think we would want to handle this problem AFTER taking max of total fac and set the num reported rather than the total
    # when health facilities reporting is greater than total health facilities, change health facilities total to = health facilities reporting
      # ameliaDT[healthFacilities_numReported > healthFacilities_total, healthFacilities_total:=healthFacilities_numReported]
      
# Number of health facilities reporting should not be greater than the total number of health facilities (but occasionally it is)...
  # when the number of health facilities reporting is greater than total health facilities:
    # if the number of health facilities reporting is equal to the max of health facilities total by health zone/year, then set
      # health facilities total = health facilities max
    ameliaDT <- ameliaDT[, healthFacilities_max := max(healthFacilities_total, na.rm=TRUE), by=c("dps", "health_zone", "year")]
    ameliaDT <- ameliaDT[healthFacilities_max == "-Inf", healthFacilities_max:=NA]  # result was -Inf where all values were missing by unique group
    ameliaDT[healthFacilities_numReported == healthFacilities_max, healthFacilities_total:= healthFacilities_max]
    # if health facilities total = health

# when healthFacilities_total variable is missing for a given year set it to be the same as the following year (since it is mostly earlier years missing)
  test <- ameliaDT[, .(healthFacilities_max = max(healthFacilities_total, na.rm=TRUE)), by=c("dps", "health_zone", "year")]
  test <- test[healthFacilities_max == "-Inf", healthFacilities_max:=NA]  # result was -Inf where all values were missing by group
  # order by year descending for use of na.locf later 
  test <- test[order(dps, health_zone, -year),]
  
  # warning - na.locf won't work if all health facilities total data is missing in most recent year
  # manually set mabalako 2017 so that it works
  test[health_zone=="mabalako" & year==2017, healthFacilities_max:=28]
  #na.locf replaces an NA with the most recent non NA prior to it
  test[, healthFacilities_max:= na.locf(healthFacilities_max), by=health_zone]

# TO DO - go back and add this in?? Ask David (1/22/19)         
      # # in some cases the mode facilities occurs 7 of 8 times and there's one random outlier value.  We want to change those
      # test[ , healthFacilities_mode := getmode(healthFacilities_max), by= c("dps", "health_zone")]
      # test2 <- test[healthFacilities_max == healthFacilities_mode, .(num_yrs_equal_to_mode = .N), by= c("dps", "health_zone")]
      # test <- merge(test, test2, by=c("dps", "health_zone"), all=TRUE)
      
    # merge test back to ameliaDT
      ameliaDT <- merge(ameliaDT, test, all.x=TRUE, by=c("dps", "health_zone", "year"))
      
      # save a subset of ameliaDT with just health facilities data for use in another script (to make presentation figures for TERG meeting)
      # forTERG <- ameliaDT[, c(1:5, 56:58, 108:109)]

# TO DO - clarify this is what we wanted with David - also what would the direction of bias be here?      
    # if health facilities total is missing, set it to be health facilities max (the max # of facilites in that year?)
      ameliaDT[is.na(healthFacilities_total), healthFacilities_total := healthFacilities_max] 
    # 1/22/19 changed this to divide reporting facilities by total facilities... make sure this is what we should do. 
      ameliaDT$healthFacilitiesProportion <- ameliaDT[, .(healthFacilities_numReported / healthFacilities_total)]
# ----------------------------------------------   


# ---------------------------------------------- 
# Standardize Test Results
  # combine age groups for tests to account for where these are separated out in different years of data- check with David, is this okay? best way to do this?
    ameliaDT[, RDT_completed := ifelse( year <= 2014, RDT_completed, (RDT_completedUnder5 + RDT_completed5andOlder))]
    ameliaDT[, RDT_positive := ifelse( year <= 2014, RDT_positive, (RDT_positiveUnder5 + RDT_positive5andOlder))]
    ameliaDT[, smearTest_completed := ifelse( year <= 2014, smearTest_completed,(smearTest_completedUnder5 + smearTest_completed5andOlder))]
    ameliaDT[, smearTest_positive := ifelse( year <= 2014, smearTest_positive, (smearTest_positiveUnder5 + smearTest_positive5andOlder))]
    
    ameliaDT <- ameliaDT[, -c("year", "smearTest_completedUnder5", "smearTest_completed5andOlder", "smearTest_positiveUnder5", "smearTest_positive5andOlder",
                              "RDT_positive5andOlder", "RDT_positiveUnder5", "RDT_completedUnder5", "RDT_completed5andOlder")]
# ----------------------------------------------     
    
    
# ----------------------------------------------   
  # rectangularize the data so there is an observation for every health zone and date - but we don't want to back cast years where the health zone
    # might not have existed. 

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