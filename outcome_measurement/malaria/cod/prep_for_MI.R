# ----------------------------------------------
  # Audrey Batzel
  #
  # 6/15/18
  # prep of PNLP data for multiple imputation
    setwd('C:/local/gf/')
  # Cleaned up/edited/redone - 02/19 through 5/19

  # TO DO:
  # Check with David:
  #     - Deterministic imputation of health facilities: I played around with this a bit to make sure that the proportion of health facilities reporting is always less than 1
  #         I was going to try to make it so that some cases were handled more individually, but that was a bit too complicated, so various parts have been commented out.
  #         Decided just to set cases where num reported > total to NA for both, and deterministically impute total.  Once total is deterministically imputed, this creates more
  #         problem cases that were previously NA.  Therefore, I did essentially the same thing, but just set num reported to NA.
  #     - No longer rectangularizing by health_zone - month.  In the raw data, there are some health zones that don't show up in different years.  I matched the ones I could on name
  #         but there are still some that didn't match up.  I can rectangularize these if we want to, but maybe it's safer to assume they didn't exist where they didn't report any
  #         data for the entire year/didn't exist in the data sheets we received. 
  #     - Had made corrections to avoid over-imputing, but check these, because I think it might be wrong.
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
dir_prepped <-"J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/archive/"

# input files
fullData <- "fullData_dps_standardized.csv"

# output files
output_dt <- "PNLP_dt_forMI_updated_3_11_19.rds"

# dt = fread(paste0( dir_prepped, "PNLP_2010to2017_prepped.csv"), stringsAsFactors = FALSE)
# dt = readRDS(paste0('J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/post_imputation/archive/imputedData_run2_agg_hz.rds'))
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
# take a subset of fullData that will be used in MI
# ---------------------------------------------- 
    # further outlier removal - using QR:
    
    # noticed problem with duplicate values:
    ind = names(ameliaDT)[ grepl(names(ameliaDT), pattern = "ASAQ")]
    ind = ind[1:8]
    id_vars = c('health_zone', 'dps', 'date')

    example = ameliaDT[, c(id_vars, ind), with = FALSE]
    
    duplicates = example[ duplicated(example[, ind, with = FALSE])]
    # remove rows of all NA
    duplicates = duplicates[ rowSums(is.na(duplicates)) != 8]
    # remove rows of all 0s
    duplicates = duplicates[  rowSums(duplicates[, -1:-3]) != 0,  ]

    duplicates_count = duplicates[ duplicated(duplicates[, ind, with = FALSE])]
    duplicates_count2 = duplicates_count[ duplicated(duplicates_count[, ind, with = FALSE])]
    duplicates_count3 = duplicates_count2[ duplicated(duplicates_count2[, ind, with = FALSE])]
# ---------------------------------------------- 
    
# ---------------------------------------------- 
# Deterministically impute total health facilities and convert the number reporting to a proportion over the total, then drop the original variable for number reporting and impute
    # the proportion so that we can back calculate the number reporting and it will always be less than the total number of facilities.
# ----------------------------------------------   
ameliaDT[, year:= year(date)]
    
  ameliaDT$healthFacilities_total <- as.numeric(ameliaDT$healthFacilities_total)
  ameliaDT$healthFacilities_numReported <- as.numeric(ameliaDT$healthFacilities_numReported)
# keep track of what the original values are before we change them:
  ameliaDT[, healthFacilities_total_orig := healthFacilities_total]
  ameliaDT[, healthFacilities_numReported_orig := healthFacilities_numReported]
      
# Number of health facilities reporting should not be greater than the total number of health facilities but there are 165 instances of this.
  # ---------------- 
   # NOTE: commented out 01/19 because I think we would want to handle this problem AFTER taking max of total fac and set the num reported rather than the total
      # # when health facilities reporting is greater than total health facilities, change health facilities total to = health facilities reporting
      # ameliaDT[healthFacilities_numReported > healthFacilities_total, healthFacilities_total:=healthFacilities_numReported]
  # ----------------
   # NOTE: commented out 3/11 because I don't think this is the right approach either.. might be best to set both to NA and then just deterministically impute health facilities
   # total the same way
    # # when the number of health facilities reporting is greater than total health facilities:
    #   # if the number of health facilities reporting is equal to the max of health facilities total by health zone/year, then set
    #     # health facilities total = health facilities max
    #   ameliaDT <- ameliaDT[, healthFacilities_max := max(healthFacilities_total, na.rm=TRUE), by=c("dps", "health_zone", "year")]
    #   ameliaDT <- ameliaDT[healthFacilities_max == "-Inf", healthFacilities_max:=NA]  # result was -Inf where all values were missing by unique group
    #   ameliaDT[healthFacilities_numReported == healthFacilities_max, healthFacilities_total:= healthFacilities_max]
    #   # if health facilities total 
  # ----------------  
    # set both health facilities reporting and health facilities total to missing when reporting > total
    ameliaDT[healthFacilities_numReported > healthFacilities_total, c('healthFacilities_total', 'healthFacilities_numReported') := NA]

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

# TO DO - clarify this is what we wanted with David - also what would the direction of bias be here?    
    
    # if health facilities total is missing, set it to be health facilities max (the max # of facilites in that year?)
      ameliaDT[is.na(healthFacilities_total), healthFacilities_total := healthFacilities_max] 
    # This created 66 new cases where num reporting > total facilities - in all of these cases, the original total facilities was NA
      # set these to be NA too:
      ameliaDT[ healthFacilities_numReported > healthFacilities_total, healthFacilities_numReported := NA]
    # 1/22/19 changed this to divide reporting facilities by total facilities... make sure this is what we should do. 
      ameliaDT[, healthFacilitiesProportion := ( healthFacilities_numReported / healthFacilities_total )]
      if ( ameliaDT[healthFacilitiesProportion > 1, .N] > 0 ) stop("Proportion of health facilities reporting must be 1.0 or less")
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
# 3/11/19 Commenting this out because we don't want to back cast years where the health zone might not have existed. 
  # I double checked this against the drc geographies map that I made last year, and there is the correct number of rows (49,320) for
  # health zones 
    
  # # rectangularize the data so there is an observation for every health zone and date
  #   hzDPS <- unique(ameliaDT[, .(dps, health_zone)])
  #   dates <- unique(ameliaDT$date)
  #   
  #   rect <- hzDPS[rep(1:nrow(hzDPS), length(unique(dates)))]
  #   rect[, date:=rep(dates, each=nrow(hzDPS))]
  #   
  #   dt <- merge(ameliaDT, rect, by=c("date", "province11_name", "health_zone", "dps"), all=TRUE)
    
  # add an id column
    ameliaDT[, id:= .I]
# ----------------------------------------------

# ----------------------------------------------  
# remove vars not needed for imputation - we will impute the proportion of health facilities reporting, and then use that to back-calculate the number reporting
    ameliaDT[, c("healthFacilities_total_orig", "healthFacilities_numReported_orig", "healthFacilities_numReported", "healthFacilities_max", "healthFacilities_numReportedWithinDeadline"):=NULL]
# ----------------------------------------------     

# ----------------------------------------------  
  # export data  
  saveRDS(ameliaDT, paste0(dir_prepped, output_dt))
# ---------------------------------------------- 