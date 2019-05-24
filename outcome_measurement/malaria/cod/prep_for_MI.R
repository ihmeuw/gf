# ----------------------------------------------
  # Audrey Batzel
  #
  # 6/15/18
  # prep of PNLP data for multiple imputation
    setwd('C:/local/gf/')
  # Cleaned up/edited/redone - 02/19 through 5/19
  # 5/16/19 reorganized code - this is now another prep file before QR/duplicate removal.  Then run duplicate removal/QR load those into
  # final_prep_for_MI.R and that's where determinisitc imputation will happen.

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
library(parallel)
# --------------------

# ----------------------------------------------
# Overview - Files and Directories
# ---------------------------------------------- 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir_prepped = paste0(root, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/archive/")
dir = paste0(root, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/")
  
# input files
fullData = "fullData_dps_standardized.csv"

# output files
dt_for_dup_removal = "ameliaDT_with_index_for_dup_removal.rds"
output_for_qr = "pnlp_for_qr.rds"

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
# subset columns to only necessary ones
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
# save DTs for outlier removal and duplicate removal to run both on the cluster
# ---------------------------------------------- 
# melt long to run qr on the data
dt = melt.data.table(ameliaDT, id.vars = id_vars, variable.factor = FALSE)
  
# save a copy of the data table for use in QR:
saveRDS(dt, paste0(dir, output_for_qr))

# create an id in ameliaDT
ameliaDT$id <- seq(1:nrow(ameliaDT))
id_vars = c(id_vars, 'id')

# save data with index
saveRDS(ameliaDT, paste0(dir, dt_for_dup_removal))
# ---------------------------------------------- 
  